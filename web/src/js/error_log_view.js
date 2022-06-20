"use strict";

var customersMap = new Map();
var usersMap = new Map();
var errorLogEntriesMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("25"); // 25 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "elid";
const TBID = "t_errorLog";
const EDIT_FORM = "f_edit_form";
const ADD_FORM = "f_add_form";
const DATALISTS = [
  {dlid: "list_customers", inputId: "f_customer", isEdit: false},
  {dlid: "list_customersEdit", inputId: "f_customerEdit", isEdit: true},
  {dlid: "list_users", inputId: "f_user", isEdit: false},
  {dlid: "list_usersEdit", inputId: "f_userEdit", isEdit: true},
];
const CHECKS = [
  {btnid: "f_isResolvedButton", inpid: "f_isResolved"},
  {btnid: "f_isResolvedButtonEdit", inpid: "f_isResolvedEdit"},
  {btnid: "f_isRecordedIn90Button", inpid: "f_isRecordedIn90"},
  {btnid: "f_isRecordedIn90ButtonEdit", inpid: "f_isRecordedIn90Edit"},
  {btnid: "f_isRanddButton", inpid: "f_isRandd"},
  {btnid: "f_isRanddButtonEdit", inpid: "f_isRanddEdit"},
];
const SUBVIEWS = [
  {id: TBID, displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editErrorLogEntryOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const confirmErrorLogEntryDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this error log entry? All data will be lost.`,
  deleteErrorLogEntry);

popSidebar("Error Log");

// init
Promise.all([
  listErrorLogEntries(),
])
  .then(() => {
    successAlert("Error log view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      populateCustomerList(),
      populateUserList(),
    ])
    .catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addErrorLogEntry);
setFormListenerWrapper(EDIT_FORM, editErrorLogEntry);
CHECKS.forEach(setClickListenerCheckboxWrapper("Yes", "No"));
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listErrorLogEntries, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listErrorLogEntries() {
  return API.listErrorLogEntries(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function populateCustomerList() {
  return API.listCustomers()
    .then((cs) => {
      customersMap = new Map(cs.map((c) => [c.name, c]));
      setupCustomerDatalists([DATALISTS[0], DATALISTS[1]], customersMap);
      popListsInner([DATALISTS[0], DATALISTS[1]], cs);
      return;
    })
    .catch(errorAlert);
};

function populateUserList() {
  return API.listUsers()
    .then((us) => {
      const updatedList = us.map((u) => {
        u.name = `${u.first} ${u.last}`;
        return u;
      });
      usersMap = new Map(updatedList.map((u) => [u.name, u]));
      setupUserDatalists([DATALISTS[2], DATALISTS[3]], usersMap);
      popListsInner([DATALISTS[2], DATALISTS[3]], updatedList);
      return;
    })
    .catch(errorAlert);
};

function popTable(els) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  els.forEach((el) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${el.id}</td>
      <td>${el.orderNumber}</td>
      <td>${el.customer.name}</td>
      <td>${el.code}</td>
      <td>${el.reportedBy.first} ${el.reportedBy.last}</td>
      <td>${el.assignedTo}</td>
      <td>${el.entryDate}</td>
      <td>${el.occurrenceDate}</td>
      ${renderBoolTD(el.isResolved)}
      ${renderBoolTD(el.isRecordedIn90)}
      ${renderBoolTD(el.isRandd)}
      <td><button class="table-button"  onclick="editErrorLogEntryOpen(${el.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmErrorLogEntryDeletion(${el.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  errorLogEntriesMap = new Map(els.map((el) => [el.id, el]));
  return;
};

function addErrorLogEntryOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addErrorLogEntry(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    customer: {},
    reportedBy: { // user obj
      password: "",
    },
  };
  return API.addErrorLogEntry(mkErrorLogEntry(fd, baseItem))
    .then(() => {
      closeDialogs();
      event.target.reset();
      successAlert("New error log entry added.");
      mainOpen();
      return listErrorLogEntries();
    })
    .catch(errorAlert);
};

function editErrorLogEntryOpen(elid) {
  if (elid === null || elid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, elid]]);
  return fillErrorLogEntry(elid);
};

function fillErrorLogEntry(elid) {
  const el = errorLogEntriesMap.get(elid);
  [ {id: "f_customerEdit", val: el.customer.name},
    {id: "f_customerIdEdit", val: el.customer.id},
    {id: "f_customerNameEdit", val: el.customer.name},
    {id: "f_customerCompanyEdit", val: el.customer.company},
    {id: "f_customerTypeEdit", val: el.customer.type},
    {id: "f_customerMarkupEdit", val: el.customer.markup},
    {id: "f_userEdit", val: `${el.reportedBy.first} ${el.reportedBy.last}`},
    {id: "f_userIdEdit", val: el.reportedBy.id},
    {id: "f_userRoleEdit", val: el.reportedBy.role},
    {id: "f_userUsernameEdit", val: el.reportedBy.username},
    {id: "f_userLastEdit", val: el.reportedBy.last},
    {id: "f_userFirstEdit", val: el.reportedBy.first},
    {id: "f_orderNumberEdit", val: el.orderNumber},
    {id: "f_occurrenceDateEdit", val: (new Date(el.occurrenceDate)).toISOString().slice(0,10)},
    {id: "f_issueNoteEdit", val: el.issueNote},
    {id: "f_resolutionNoteEdit", val: el.resolutionNote},
    {id: "f_rootCauseNoteEdit", val: el.rootCauseNote},
    {id: "f_isResolvedEdit", val: el.isResolved, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[1].btnid},
    {id: "f_isRecordedIn90Edit", val: el.isRecordedIn90, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[3].btnid},
    {id: "f_isRanddEdit", val: el.isRandd, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[5].btnid},
    {id: "f_randdCostEdit", val: el.randdCost},
    {id: "f_idEdit", val: el.id},
    {id: "f_codeEdit", val: el.code, isSelect: true},
    {id: "f_assignedToEdit", val: el.assignedTo, isSelect: true},
  ].forEach(setInputVal);
  return;
};

function editErrorLogEntry(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const elid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(elid)) {
    errorAlert("Invalid error log entry id provided. Contact system admin.");
    return;
  }
  return API.editErrorLogEntry(elid, mkErrorLogEntry(fd))
    .then(() => {
      event.target.reset();
      successAlert("Error log entry edit complete.");
      mainOpen();
      return listErrorLogEntries();
    })
    .catch(errorAlert);
};

function mkErrorLogEntry(fd, baseItem=null) {
  const defObjShape = { customer: {}, reportedBy: { password: ""} };
  const cif = "customer";
  const uif = "reportedBy";
  const baseFields = [
    {f: "customerName", actual: "name", isNested: true, intField: cif},
    {f: "customerId", actual: "id", parser: safeParseInt, isNested: true, intField: cif},
    {f: "customerCompany", actual: "company", isNested: true, intField: cif},
    {f: "customerType", actual: "type", isNested: true, intField: cif},
    {f: "customerMarkup", actual: "markup", parser: safeParseDouble, isNested: true, intField: cif},
    {f: "userId", actual: "id", parser: safeParseInt, isNested: true, intField: uif},
    {f: "userFirst", actual: "first", isNested: true, intField: uif},
    {f: "userLast", actual: "last", isNested: true, intField: uif},
    {f: "userUsername", actual: "username", isNested: true, intField: uif},
    {f: "userRole", actual: "role", isNested: true, intField: uif},
    {f: "orderNumber"},
    {f: "occurrenceDate", parser: safeParseDate},
    {f: "entryDate", actual: "entryDate", parser: safeParseDate},
    {f: "issueNote"},
    {f: "resolutionNote"},
    {f: "rootCauseNote"},
    {f: "code"},
    {f: "assignedTo"},
    {f: "isResolved", parser: safeParseBool},
    {f: "isRecordedIn90", parser: safeParseBool},
    {f: "isRandd", parser: safeParseBool},
    {f: "randdCost", parser: safeParseMaybeDouble},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deleteErrorLogEntry(elid) {
  return API.deleteErrorLogEntry(elid)
    .then(() => {
      closeDialogs();
      successAlert("Error log entry removed.");
      return listErrorLogEntries();
    })
    .catch((e) => {
      closeDialogs();
      return errorAlert(e);
    });
};

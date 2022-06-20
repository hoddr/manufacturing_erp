"use strict";

var customersMap = new Map();
var usersMap = new Map();
var feedbackComplaintsMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("25"); // 25 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "fcid";
const TBID = "t_complaints";
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
  {btnid: "f_isAppUpdatedButton", inpid: "f_isAppUpdated"},
  {btnid: "f_isAppUpdatedButtonEdit", inpid: "f_isAppUpdatedEdit"},
  {btnid: "f_isRecordedIn90Button", inpid: "f_isRecordedIn90"},
  {btnid: "f_isRecordedIn90ButtonEdit", inpid: "f_isRecordedIn90Edit"},
  {btnid: "f_isRanddButton", inpid: "f_isRandd"},
  {btnid: "f_isRanddButtonEdit", inpid: "f_isRanddEdit"},
];
const SUBVIEWS = [
  {id: "t_complaints", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editFeedbackComplaintOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);

popSidebar("Feedback Complaints");

// init
Promise.all([
  listFeedbackComplaints(),
])
  .then(() => {
    successAlert("Feedback complaints view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      populateCustomerList(),
      populateUserList(),
    ])
    .catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addFeedbackComplaint);
setFormListenerWrapper(EDIT_FORM, editFeedbackComplaint);
CHECKS.forEach(setClickListenerCheckboxWrapper("Yes", "No"));
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listFeedbackComplaints, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listFeedbackComplaints() {
  return API.listFeedbackComplaints(searchQS, limitQS, pageQS)
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

function popTable(fcs) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  fcs.forEach((fc) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${fc.id}</td>
      <td>${fc.orderNumber}</td>
      <td>${fc.customer.name}</td>
      <td>${fc.assignedTo.first} ${fc.assignedTo.last}</td>
      <td>${fc.occurrenceDate}</td>
      ${renderBoolTD(fc.isResolved)}
      ${renderBoolTD(fc.isAppUpdated)}
      ${renderBoolTD(fc.isRecordedIn90)}
      ${renderBoolTD(fc.isRandd)}
      <td onclick="editFeedbackComplaintOpen(${fc.id});"><i class="fa fa-pencil"></i></td>
      <td><button class="table-button" onclick="confirmFeedbackComplaintDeletion(${fc.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  feedbackComplaintsMap = new Map(fcs.map((fc) => [fc.id, fc]));
  return;
};

function addFeedbackComplaintOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addFeedbackComplaint(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    customer: {},
    assignedTo: { // user obj
      password: "",
    },
    randdCost: null,
  };
  infoAlert("Sending new feedback complaint and sending email. This will take a few seconds.");
  return API.addFeedbackComplaint(mkFeedbackComplaint(fd, baseItem))
    .then(() => {
      closeDialogs();
      event.target.reset();
      successAlert("New feedback complaint added.");
      mainOpen();
      return listFeedbackComplaints();
    })
    .catch(errorAlert);
};

function editFeedbackComplaintOpen(fcid) {
  if (fcid === null || fcid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, fcid]]);
  return fillFeedbackComplaint(fcid);
};

function fillFeedbackComplaint(fcid) {
  const fc = feedbackComplaintsMap.get(fcid);
  [ {id: "f_customerEdit", val: fc.customer.name},
    {id: "f_customerIdEdit", val: fc.customer.id},
    {id: "f_customerNameEdit", val: fc.customer.name},
    {id: "f_customerCompanyEdit", val: fc.customer.company},
    {id: "f_customerTypeEdit", val: fc.customer.type},
    {id: "f_customerMarkupEdit", val: fc.customer.markup},
    {id: "f_userEdit", val: `${fc.assignedTo.first} ${fc.assignedTo.last}`},
    {id: "f_userIdEdit", val: fc.assignedTo.id},
    {id: "f_userRoleEdit", val: fc.assignedTo.role},
    {id: "f_userUsernameEdit", val: fc.assignedTo.username},
    {id: "f_userLastEdit", val: fc.assignedTo.last},
    {id: "f_userFirstEdit", val: fc.assignedTo.first},
    {id: "f_orderNumberEdit", val: fc.orderNumber},
    {id: "f_occurrenceDateEdit", val: (new Date(fc.occurrenceDate)).toISOString().slice(0,10)},
    {id: "f_issueNoteEdit", val: fc.issueNote},
    {id: "f_resolutionNoteEdit", val: fc.resolutionNote},
    {id: "f_isResolvedEdit", val: fc.isResolved, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[1].btnid},
    {id: "f_isAppUpdatedEdit", val: fc.isAppUpdated, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[3].btnid},
    {id: "f_isRecordedIn90Edit", val: fc.isRecordedIn90, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[5].btnid},
    {id: "f_isRanddEdit", val: fc.isRandd, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[7].btnid},
    {id: "f_randdCostEdit", val: fc.randdCost},
    {id: "f_idEdit", val: fc.id},
    {id: "f_displayId", val: fc.id},
  ].forEach(setInputVal);
  return;
};

function editFeedbackComplaint(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const fcid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(fcid)) {
    errorAlert("Invalid feedback complaint id provided. Contact system admin.");
    return;
  }
  return API.editFeedbackComplaint(fcid, mkFeedbackComplaint(fd))
    .then(() => {
      event.target.reset();
      successAlert("Feedback complaint edit complete.");
      mainOpen();
      return listFeedbackComplaints();
    })
    .catch(errorAlert);
};

function mkFeedbackComplaint(fd, baseItem=null) {
  const defObjShape = { customer: {}, assignedTo: { password: ""} };
  const cif = "customer";
  const uif = "assignedTo";
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
    {f: "occurrenceDate", actual: "entryDate", parser: safeParseDate},
    {f: "issueNote"},
    {f: "resolutionNote"},
    {f: "isResolved", parser: safeParseBool},
    {f: "isAppUpdated", parser: safeParseBool},
    {f: "isRecordedIn90", parser: safeParseBool},
    {f: "isRandd", parser: safeParseBool},
    {f: "randdCost", parser: safeParseMaybeDouble},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deleteFeedbackComplaint(fcid) {
  return API.deleteFeedbackComplaint(fcid)
    .then(() => {
      closeDialogs();
      successAlert("Feedback complaint removed.");
      return listFeedbackComplaints();
    })
    .catch((e) => {
      closeDialogs();
      return errorAlert(e);
    });
};

const confirmFeedbackComplaintDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this feedback complaint? All data will be lost.`,
  deleteFeedbackComplaint);

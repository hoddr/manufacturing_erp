"use strict";

var customersMap = new Map();
var currentCustomer;
var addressMap = new Map();
var setFn;
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "cid";
const TBID = "t_customers";
const EDIT_FORM = "f_edit_customer";
const ADD_FORM = "f_add_customer";
const ADDRESS_FORM = "f_set_address";
const SUBVIEWS = [
  {id: "d_customers", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editCustomerOpen)},
  {id: "d_sub_address", displayStyle: "flex", viewKey: "address", fn: getQSId(IDKEY, editAddressOpen)},
];
const DATALISTS = [
  {dlid: "list_address", inputId: "f_address", isEdit: false},
];
const CHECKS = [
  {btnid: "f_isTaxExemptButton", inpid: "f_isTaxExempt"},
  {btnid: "f_isTaxExemptButtonEdit", inpid: "f_isTaxExemptEdit"},
];

const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const setAddressPartial = setAddressWrapper(ADDRESS_FORM, API.setCustomerToAddress);
const confirmToggleTaxExemptStatus = confirmToggleTaxExemptWrapper(cbConfirmTaxExemptToggle);

popSidebar("Customers");

// init
Promise.all([
  listCustomers(),
  listAddresses(DATALISTS, addressMap),
  ])
  .then(([hole, newMap]) => {
    addressMap = newMap;
    successAlert("Customers view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addCustomer);
setFormListenerWrapper(EDIT_FORM, editCustomer);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listCustomers, event) });
CHECKS.forEach(setClickListenerCheckboxWrapper("Yes", "No"));

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listCustomers() {
  return API.listCustomers(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};


function popTable(cs) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  cs.forEach((c) => {
    if (c.name !== "_default") {
      const nextRow = tb.insertRow(-1);
      const markup = `
        <td>${c.id}</td>
        <td>${c.name}</td>
        <td>${renderBlankCol(c.company)}</td>
        <td>${renderBlankCol(c.type)}</td>
        <td>${c.markup}</td>
        ${renderTaxExemptStatusTD(c.isTaxExempt, c.id)}
        <td onclick="editCustomerOpen(${c.id});"><i class="fa fa-pencil"></i></td>
        <td onclick="editAddressOpen(${c.id});"><i class="fa fa-pencil"></i></td>
      `;
      nextRow.innerHTML = markup;
    }
  });
  customersMap = new Map(cs.map((c) => [c.id, c]));
  currentCustomer = null;
  return;
};

function addCustomerOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addCustomer(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0,
    isTaxExempt: false,
  };
  return API.addCustomer(mkCustomer(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New customer added.");
      mainOpen();
      return listCustomers();
    })
    .catch(errorAlert);
};

function editCustomerOpen(cid) {
  if (cid === null || cid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, cid]]);
  return fillEditCustomer(cid);
};

function fillEditCustomer(cid) {
  const c = customersMap.get(cid);
  [ {id: "f_nameEdit", val: c.name},
    {id: "f_companyEdit", val: c.company},
    {id: "f_typeEdit", val: c.type},
    {id: "f_markupEdit", val: c.markup},
    {id: "f_idEdit", val: c.id},
    {id: "f_addressAssignId", val: c.id},
    {id: "f_isTaxExemptEdit", val: c.isTaxExempt, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[1].btnid},
  ].forEach(setInputVal);
  currentCustomer = c;
  return;
};

function editCustomer(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  return API.editCustomer(currentCustomer.id, mkCustomer(fd))
    .then(() => {
      event.target.reset();
      successAlert("Customer edit complete.");
      mainOpen();
      return listCustomers();
    })
    .catch(errorAlert);
};

function editAddressOpen(cid) {
  if (cid === null || cid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  setQueryString([[IDKEY, cid]]);
  infoAlert("Fetching customer address...");
  return API.getCustomerAddress(cid)
    .then((a) => {
      closeDialogs();
      successAlert("Customer address retrieved.");
      return fillEditAddress(cid, a);
    })
    .catch(errorAlert);
};

function fillEditAddress(cid, a) {
  if (!a) {
    return;
  }
  const c = customersMap.get(cid);
  [ {id: "f_addressAssignId", val: c.id},
    {id: "f_addressId", val: a ? a.id || null : null},
    {id: "f_address", val: makeAddressString(a)},
  ].forEach(setInputVal);
  currentCustomer = c;
  const n = document.getElementById(ADDRESS_FORM);
  if (setFn) {
    n.removeEventListener("submit", setFn);
  }
  setFn = setAddressPartial(addressMap);
  n.addEventListener("submit", setFn);
  return;
};

function mkCustomer(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "name"},
    {f: "company"},
    {f: "type"},
    {f: "markup", parser: safeParseDouble},
    {f: "isTaxExempt", parser: safeParseBool},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function renderTaxExemptStatusTD(val, id) {
  const s = `confirmToggleTaxExemptStatus(${id}, ${val})`;
  return val ?
    `<td onclick="${s};" style="color: green;"><i class="fa fa-check"></i></td>` :
    `<td onclick="${s};" style="color: red;"><i class="fa fa-times"></i></td>`;
};

function cbConfirmTaxExemptToggle(id, isTaxExempt) {
  return function inner() {
    closeDialogs();
    var apifn;
    var msg;
    if (isTaxExempt) {
      apifn = API.setCustomerToNotTaxExempt;
      msg = "Customer set to not tax exempt.";
    } else {
      apifn = API.setCustomerToTaxExempt;
      msg = "Customer set to tax exempt.";
    }
    return apifn(id)
      .then(() => {
        successAlert(msg);
        return listCustomers();
      })
      .catch(errorAlert);
  };
};

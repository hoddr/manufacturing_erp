"use strict";

var quotesMap = new Map();
var customersMap = new Map();
var addressMap = new Map();
var currentQuote;
var setFn;
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "pid";
const TBID = "t_quotes";
const ADDRESS_FORM = "f_set_address";
const SUBVIEWS = [
  {id: "t_quotes", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editQuoteOpen)},
  {id: "d_sub_address", displayStyle: "flex", viewKey: "address", fn: getQSId(IDKEY, editAddressOpen)},
];
const DATALISTS = [
  {dlid: "list_customers", inputId: "f_customer", isEdit: false},
  {dlid: "list_customersEdit", inputId: "f_customerEdit", isEdit: true},
  {dlid: "list_address", inputId: "f_address", isEdit: false},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const confirmToggleConfirmStatus = confirmToggleConfirmStatusWrapper(toggleConfirm);
const setAddressPartial = setAddressWrapper(ADDRESS_FORM, API.setCCQuoteToAddress);
popSidebar("CC");

// init
Promise.all([
  listQuotes(),
  listAddresses([DATALISTS[2]], addressMap),
])
  .then(([hole, newMap]) => {
    addressMap = newMap;
    successAlert("CC quotes view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return populateCustomerList();
  })
  .catch(errorAlert);

setFormListenerWrapper("f_quote", addQuote);
setFormListenerWrapper("f_edit_quote", editQuote);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listQuotes, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listQuotes() {
  return API.listCCQuotes(searchQS, limitQS, pageQS)
    .then(popQuoteTable)
    .catch(errorAlert);
};

function popQuoteTable(qs) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  qs.forEach((q) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${q.id}</td>
      <td><a href="<<BASE_URL>>/views/cc_quote_view.html?pid=${q.pid}">AT-${q.pid}</a></td>
      <td>${q.customer.name}</td>
      <td>$${formatPrice(q.shippingCosts)}</td>
      <td>$${formatPrice(q.fastPassCost)}</td>
      ${renderConfirmedStatusTD(q.isConfirmed, q.pid)}
      <td><button class="table-button" onclick="editAddressOpen(${q.pid});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="editQuoteOpen(${q.pid});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmQuoteDeletion(${q.pid});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  quotesMap = new Map(qs.map((q) => [q.pid, q]));
  return;
};

function populateCustomerList() {
  return API.listCustomers()
    .then((cs) => {
      customersMap = new Map(cs.map((c) => [c.name, c]));
      setupCustomerDatalists([DATALISTS[0], DATALISTS[1]], customersMap);
      popListsInner([DATALISTS[0], DATALISTS[1]], cs);
    })
    .catch(errorAlert);
};

function openAddQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  return;
};

function addQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    customer: {},
    isConfirmed: false,
    curbs: [],
  };
  return API.addCCQuote(mkQuote(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New quote added.");
      mainOpen();
      return listQuotes();
    })
    .catch(errorAlert);
};

function editQuoteOpen(pid) {
  if (pid === null || pid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, pid]]);
  return fillEditQuote(pid);
};

function fillEditQuote(pid) {
  console.log(quotesMap, pid);
  const q = quotesMap.get(`${pid}`);
  [ {id: "f_pidEdit", val: q.pid},
    {id: "f_idEdit", val: q.id},
    {id: "f_shippingCostsEdit", val: q.shippingCosts},
    {id: "f_customerEdit", val: q.customer.name},
    {id: "f_customerIdEdit", val: q.customer.id},
    {id: "f_customerNameEdit", val: q.customer.name},
    {id: "f_customerCompanyEdit", val: q.customer.company},
    {id: "f_customerTypeEdit", val: q.customer.type},
    {id: "f_customerMarkupEdit", val: q.customer.markup},
    {id: "f_customerTaxExemptEdit", val: q.customer.isTaxExempt},
    {id: "f_fastPassCostEdit", val: q.fastPassCost},
  ].forEach(setInputVal);
  currentQuote = q;
  return;
};

function editQuote(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const qid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(qid)) {
    errorAlert("Invalid quote id provided. Contact system admin.");
    return;
  }
  return API.editCCQuote(qid, mkQuote(fd))
    .then(() => {
      successAlert("Quote edit complete.");
      event.target.reset();
      currentQuote = null;
      mainOpen();
      return listQuotes();
    })
    .catch(errorAlert);
};

function mkQuote(fd, baseItem=null) {
  const defObjShape = {
    customer: {},
    curbs: [],
    isConfirmed: currentQuote ? currentQuote.isConfirmed : false,
  };
  const baseFields = [
    {f: "pid"},
    {f: "shippingCosts", parser: safeParseDouble},
    {f: "fastPassCost", parser: safeParseDouble},
    {f: "customerName", actual: "name", isNested: true, intField: "customer"},
    {f: "customerId", actual: "id", parser: safeParseInt, isNested: true, intField: "customer"},
    {f: "customerCompany", actual: "company", isNested: true, intField: "customer"},
    {f: "customerType", actual: "type", isNested: true, intField: "customer"},
    {f: "customerMarkup", actual: "markup", parser: safeParseDouble, isNested: true, intField: "customer"},
    {f: "customerTaxExempt", actual: "isTaxExempt", parser: safeParseBool, isNested: true, intField: "customer"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

const confirmQuoteDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this quote and all associated curb entries? All data will be lost.`,
  deleteQuote);

function deleteQuote(pid) {
  return API.deleteCCQuote(pid)
    .then(() => {
      closeDialogs();
      successAlert("CC quote deletion successful.");
      return listQuotes();
    })
    .catch(errorAlert);
};

function toggleConfirm(pid, isConfirmed) {
  return function inner() {
    closeDialogs();
    if (isConfirmed) {
      return API.setCCQuoteToNotConfirmed(pid)
        .then(() => {
          successAlert("CC quote set to not confirmed.");
          return listQuotes();
        })
        .catch(errorAlert);
    }
    return API.setCCQuoteToConfirmed(pid)
      .then(() => {
        closeDialogs();
        successAlert("CC quote set to confirmed.");
        return listQuotes();
      })
      .catch(errorAlert);
  };
};

function editAddressOpen(pid) {
  if (pid === null || pid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  setQueryString([[IDKEY, pid]]);
  infoAlert("Fetching CC quote shipping address...");
  return API.getCCQuoteAddress(pid)
    .then((a) => {
      closeDialogs();
      successAlert("CC quote shipping address retrieved.");
      return fillEditAddress(pid, a);
    })
    .catch(errorAlert);
};

function fillEditAddress(pid, a) {
  if (!a) {
    return;
  }
  const q = quotesMap.get(`${pid}`);
  [ {id: "f_addressAssignId", val: q.id},
    {id: "f_addressId", val: a ? a.id || null : null},
    {id: "f_address", val: makeAddressString(a)},
  ].forEach(setInputVal);
  currentQuote = q;
  const n = document.getElementById(ADDRESS_FORM);
  if (setFn) {
    n.removeEventListener("submit", setFn);
  }
  setFn = setAddressPartial(addressMap);
  n.addEventListener("submit", setFn);
  return;
};

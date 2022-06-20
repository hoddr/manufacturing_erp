"use strict";

var quotesMap = new Map();
var customersMap = new Map();
var currentQuote;
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "qid";
const TBID = "t_quotes";
const TBID2 = "t_lineItems";
const SUBVIEWS = [
  {id: "t_quotes", displayStyle: "", viewKey: "main"},
  {id: "t_lineItems", displayStyle: "", viewKey: "lineItems", fn: getQSId(IDKEY, openLineItems)},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editQuoteOpen)},
];
const DATALISTS = [
  {dlid: "list_customers", inputId: "f_customer", isEdit: false},
  {dlid: "list_customersEdit", inputId: "f_customerEdit", isEdit: true},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const popLineItemTable = popLineItemTableWrapper(TBID2);
popSidebar("Quotes");

// init
Promise.all([
  listQuotes(),
])
  .then(() => {
    successAlert("Quotes view data loading complete.");
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
  return API.listQuoteSkeletons(searchQS, limitQS, pageQS)
    .then(popQuoteTable)
    .catch(errorAlert);
};

function popQuoteTable(qs) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  qs.forEach((q) => {
    const nextRow = tb.insertRow(-1);
    const total = `$${formatPrice(q.total)}`;
    const setPrice = q.setPrice ? `$${formatPrice(q.setPrice)}` : " - ";
    const lineItemButton = q.type === "QP" ?
      renderNullTD(null) :
      `<td><button class="table-button" onclick="openLineItems(${q.id});">(&rarr;) View line items</button></td>`;
    const markup = `
      <td>${q.type}</td>
      <td><a href="<<BASE_URL>>/views/quote_view.html?qid=${q.id}">${q.number}</a></td>
      <td>${renderBlankCol(q.quotepo)}</td>
      <td>${renderBlankCol(q.orderNumber)}</td>
      <td>${q.customer.name}</td>
      <td>${setPrice}</td>
      <td>${q.createdAt}</td>
      ${lineItemButton}
      <td><button class="table-button" onclick="editQuoteOpen(${q.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmQuoteDeletion(${q.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  quotesMap = new Map(qs.map((q) => [q.id, q]));
  return;
};

function populateCustomerList() {
  return API.listCustomers()
    .then((cs) => {
      customersMap = new Map(cs.map((c) => [c.name, c]));
      setupCustomerDatalists(DATALISTS, customersMap);
      popListsInner(DATALISTS, cs);
    })
    .catch(errorAlert);
};

function openLineItems(qid) {
  if (qid === null || qid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  return API.getQuote(qid)
    .then((q) => {
      setTableCaption("cap_lineItems", `Quote ${q.number} &rarr; Line Items`);
      popLineItemTable(q.lineItems);
      return;
    })
    .catch(errorAlert);
};

function openAddQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  return;
};

function addQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    customer: {},
    lineItems: [],
    quotepo: null,
    isWrapped: false,
    isResidentialSO: false,
    isShipKD: false,
  };
  return API.addQuote(mkQuote(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New quote added.");
      mainOpen();
      return listQuotes();
    })
    .catch(errorAlert);
};

function editQuoteOpen(qid) {
  if (qid === null || qid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  setQueryString([[IDKEY, qid]]);
  return fillEditQuote(qid);
};

function fillEditQuote(qid) {
  const q = quotesMap.get(qid);
  [ {id: "f_quoteNumberEdit", val: q.number},
    {id: "f_quoteTypeEdit", val: q.type},
    {id: "f_idEdit", val: q.id},
    {id: "f_customerEdit", val: q.customer.name},
    {id: "f_customerIdEdit", val: q.customer.id},
    {id: "f_customerNameEdit", val: q.customer.name},
    {id: "f_customerCompanyEdit", val: q.customer.company},
    {id: "f_customerTypeEdit", val: q.customer.type},
    {id: "f_customerMarkupEdit", val: q.customer.markup},
    {id: "f_customerTaxExemptEdit", val: q.customer.isTaxExempt},
    {id: "f_setPriceEdit", val: q.setPrice},
    {id: "f_quotepoEdit", val: q.quotepo},
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
  return API.editQuote(qid, mkQuote(fd))
    .then(() => {
      successAlert("Quote edit complete.");
      event.target.reset();
      mainOpen();
      return listQuotes();
    })
    .catch(errorAlert);
};

function mkQuote(fd, baseItem=null) {
  const defObjShape = {
    customer: {},
    lineItems: [],
    isWrapped: false,
    isResidentialSO: false,
    isShipKD: false,
  };
  const baseFields = [
    {f: "number"},
    {f: "type"},
    {f: "customerName", actual: "name", isNested: true, intField: "customer"},
    {f: "customerId", actual: "id", parser: safeParseInt, isNested: true, intField: "customer"},
    {f: "customerCompany", actual: "company", isNested: true, intField: "customer"},
    {f: "customerType", actual: "type", isNested: true, intField: "customer"},
    {f: "customerMarkup", actual: "markup", parser: safeParseDouble, isNested: true, intField: "customer"},
    {f: "customerTaxExempt", actual: "isTaxExempt", parser: safeParseBool, isNested: true, intField: "customer"},
    {f: "setPrice", parser: safeParseMaybeDouble},
    {f: "quotepo"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

const confirmQuoteDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this quote and all associated line item entries? All data will be lost.`,
  deleteQuote);

function deleteQuote(qid) {
  return API.deleteQuote(qid)
    .then(() => {
      closeDialogs();
      successAlert("Quote deletion successful.");
      return listQuotes();
    })
    .catch((e) => {
      closeDialogs();
      return errorAlert(e);
    });
};

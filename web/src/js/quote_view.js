"use strict";

var currentQuote;
const TBID = "t_quote";
const TBID2 = "t_lineItems";
const SUBVIEWS = [
  {id: "t_quote", displayStyle: "", viewKey: "main"}, // 0
  {id: "t_lineItems", displayStyle: "", viewKey: "lineItems", fn: getQSId("qid", openLineItems)}, // 1
  {id: "d_sub_qtop_form", displayStyle: "flex", viewKey: "toProject", fn: createProjectOpen}, // 2
  {id: "d_sub_qtoo_form", displayStyle: "flex", viewKey: "toOrder", fn: createOrderOpen}, // 3
];
const mainOpen = mainOpenWrapper(SUBVIEWS, []);
const popLineItemTable = popLineItemTableWrapper(TBID2);
popSidebar();

// init
Promise.all([
  getQuote(),
])
  .then(() => {
    successAlert("Quote subview data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper("f_quote_to_project", createProjectFromQuote);
setFormListenerWrapper("f_quote_to_order", createOrderFromQuote);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function getQuote() {
  const mqid = getQSId("qid", id => id)();
  if (!mqid) {
    errorAlert("Quote id query param not set. Re-select quote from quotes table view.");
    currentQuote = null;
    return;
  }
  return API.getQuote(mqid)
    .then((q) => {
      currentQuote = q;
      popInfo();
      return q;
    })
    .catch(errorAlert);
};

function popInfo() {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  const nextRow = tb.insertRow(-1);
  const total = `$${formatPrice(currentQuote.total)}`;
  const setPrice = currentQuote.setPrice ? `$${formatPrice(currentQuote.setPrice)}` : " - ";
  const lineItemButton = currentQuote.type === "QP" ?
    renderNullTD(null) :
    `<td><button class="table-button" onclick="openLineItems(${currentQuote.id});">(&rarr;) View line items</button></td>`;
  const markup = `
    <td>${currentQuote.type}</td>
    <td>${currentQuote.number}</td>
    <td>${currentQuote.quotepo}</td>
    <td>${renderBlankCol(currentQuote.projectNumber)}</td>
    <td>${renderBlankCol(currentQuote.orderNumber)}</td>
    <td>${currentQuote.customer.name}</td>
    <td>${setPrice}</td>
    <td>${currentQuote.setPrice ? " - " : total}</td>
    <td>${currentQuote.createdAt}</td>
    ${lineItemButton}
  `;
  nextRow.innerHTML = markup;
  return;
};

function openLineItems() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  setTableCaption("cap_lineItems", `Quote ${currentQuote.number} &rarr; Line Items`);
  popLineItemTable(currentQuote.lineItems);
  return;
};

// project from quote
function createProjectOpen() {
  if (!currentQuote) {
    infoAlert("Current quote not set. Contact system admin.");
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setTableCaption("cap_toProject", `Quote ${currentQuote.number} &rarr; To Project`);
  if (currentQuote.quotepo) {
    const n = document.getElementById("f_po");
    n.value = currentQuote.quotepo;
  }
  return;
};

function createProjectFromQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseProject = {
    id: 0, // default placeholder
    customer: currentQuote.customer,
    isActive: true,
    quoteId: currentQuote.id,
    quoteNumber: currentQuote.number,
    extras: [],
    orders: [],
  };
  return API.createProjectFromQuote(currentQuote.id, mkProject(fd, baseProject))
    .then(() => {
      // TODO could retrieve new project id and re-route to project subview
      successAlert("Project creation successful.");
      event.target.reset();
      mainOpen();
      return;
    })
    .catch(errorAlert);
};

function mkProject(fd, baseItem=null) {
  const defObjShape = { extras: [], orders: [], customer: {}, isActive: true };
  const baseFields = [
    {f: "name"},
    {f: "appId", parser: safeParseInt},
    {f: "po"},
    {f: "totalEstimatedWeight", parser: safeParseDouble},
  ];
  const editFields = [];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};


// order from quote
function createOrderOpen() {
  if (!currentQuote) {
    infoAlert("Current quote not set. Contact system admin.");
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  setTableCaption("cap_toOrder", `Quote ${currentQuote.number} &rarr; To Order`);
  if (currentQuote.quotepo) {
    const n = document.getElementById("f_ordPo");
    n.value = currentQuote.quotepo;
  }
  return;
};

function createOrderFromQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseOrder = {
    id: 0, // default placeholder
    type: "FL", // must be FL!
    lineItems: currentQuote.lineItems,
    customer: currentQuote.customer,
    isWrapped: currentQuote.isWrapped,
    isResidentialSO: currentQuote.isResidentialSO,
    isShipKD: currentQuote.isShipKD,
    isBilled: true,
  };
  return API.createOrderFromQuote(currentQuote.id, mkOrd(fd, baseOrder))
    .then(() => {
      successAlert("Order creation successful.");
      event.target.reset();
      mainOpen();
      return;
    })
    .catch(errorAlert);
};

function mkOrd(fd, baseItem=null) {
  const defObjShape = {
    isWrapped: false,
    isResidentialSO: false,
    isShipKD: false,
    isBilled: true,
  };
  const baseFields = [
    {f: "ordNumber", actual: "number"},
    {f: "ordPo", actual: "po"},
  ];
  const editFields = [];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function getQuoteReport() {
  return API.getQuoteReport(currentQuote.id)
    .then(() => {
      successAlert("Quote report generation successful.");
      return;
    })
    .catch(errorAlert);
};

function getProjectEstimate() {
  return API.getProjectEstimateReport(currentQuote.id)
    .then(() => {
      successAlert("Project estimate generation successful.");
      return;
    })
    .catch(errorAlert);
};

function getQuoteDetails() {
  return API.getDetailedQuoteData(currentQuote.id)
    .then(() => {
      successAlert("Detailed quote line items report generated.");
      return;
    })
    .catch(errorAlert);
};

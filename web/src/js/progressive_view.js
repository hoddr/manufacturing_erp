"use strict";

var currentOrder;
var lineItemsMap = new Map();
var isBeingMarked = false;
const IDKEY = "oid";
const TBID = "t_lineItems";
const SUBVIEWS = [
  {id: "t_lineItems", displayStyle: "", viewKey: "main"},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const confirmSetAsBilled = simpleDeleteConfirmationProcess(
  "This will subtract the fabricated assemblies from inventory and cannot be undone. Please verify this is the right time to do this.",
  setAsBilled, "Action canceled.");
popSidebar("");

// init
Promise.all([
  getOrder(),
])
  .then(() => {
    successAlert("Order data loading complete.");
    pseudoRoute(SUBVIEWS);
    return;
  })
  .catch(errorAlert);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function getOrder() {
  const moid = getQSId(IDKEY, (id) => id)();
  return API.getOrderById(moid)
    .then((o) => {
      currentOrder = o;
      setTableCaption("cap_lineItems", `Order #: ${o.number}, Inv. #: ${!o.invoiceNumber ? "" : o.invoiceNumber} &rarr; Line Items`);
      return popLineItems(o);
    })
    .catch(errorAlert);
};

function popLineItems(o) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  o.lineItems.forEach((li) => {
    const nextRow = tb.insertRow(-1);
    const incMarkup = li.quantFabbed < li.quantity ?
      `<td><button id="btn_inc_${li.id}" class="table-button" onclick="this.disabled=true;incrementFabQuant(${li.id})(event);">(+)</button></td>` :
      `<td> - </td>`;
    const decMarkup = li.quantFabbed > 0 ?
      `<td><button id="btn_dec_${li.id}" class="table-button" onclick="this.disabled=true;decrementFabQuant(${li.id})(event);">(-)</button></td>` :
      `<td> - </td>`;
    const markup = `
      <td>${li.id}</td>
      <td>${li.description}</td>
      <td>${li.quantity}</td>
      <td>${li.quantFabbed}</td>
      <td>$${formatPrice(li.price)}</td>
      <td>${li.weight}</td>
      ${incMarkup}
      ${decMarkup}
      ${renderBoolTD(li.isExtra)}
      ${renderBoolTD(li.isFabbed)}
    `;
    nextRow.innerHTML = markup;
  });
  lineItemsMap = new Map(o.lineItems.map((li) => [li.id, li]));
  return;
};

function incrementFabQuant(lid) {
  return function(event) {
    return alterFabQuant(lid, true, event);
  };
};

function decrementFabQuant(lid) {
  return function(event) {
    return alterFabQuant(lid, false, event);
  };
};

// flag is isAdd
function alterFabQuant(lid, flag, event) {
  if (isBeingMarked) {
    return errorAlert("Cannot perform this operation while another is in progress...");
  }
  closeDialogs();
  const btn = document.getElementById(event.target.id);
  btn.disabled = true;
  isBeingMarked = true;
  event.stopPropagation();
  event.preventDefault();
  const li = lineItemsMap.get(lid);
  return API.progressiveOrderStatus(currentOrder.id, li, flag)
    .then(() => {
      successAlert("Fabrication quantity altered.");
      isBeingMarked = false;
      btn.disabled = false;
      return getOrder();
    })
    .catch((err) => {
      errorAlert(err);
      btn.disabled = false;
      isBeingMarked = false;
    });
};

function setAsBilled() {
  const moid = getQSId(IDKEY, (id) => id)();
  closeDialogs();
  promptDialog(`Please enter the correct invoice number for the order.`, (event) => {
    event.preventDefault();
    event.stopPropagation();
    const fd = new FormData(event.target);
    event.target.reset();
    const txt = fd.get("prompt");
    if (txt === "") {
      closeDialogs();
      infoAlert("Please enter a valid invoice number.");
      return;
    }
    return API.setOrderToBilled(moid, txt)
      .then(() => {
        closeDialogs();
        successAlert("Assemblies removed from inventory for order. Set to billed.");
        return getOrder();
      })
      .catch(errorAlert);
  });
};

function getFlangeReport() {
  const moid = getQSId(IDKEY, id => id)();
  return API.getFlangeReport(moid)
    .then(writeCSV)
    .catch(errorAlert);
};

function writeCSV(d) {
  var markdown = "name,quantity\n";
  d.forEach(({name, quant}) => {
    markdown += `${name},${quant}\n`;
  });
  return promptCSVDownload([markdown], "flange_report");
};

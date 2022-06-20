"use strict";

var currentOrder;
var ordersMap = new Map();
var projectsMap = new Map();
var projectsIdMap = new Map();
var sectionsMap = new Map();
var sectionsIdMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("25"); // 25 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "oid";
const TBID = "t_orders";
const TBID2 = "t_lineItems";
const DATALISTS = [
  {dlid: "list_projectsEdit", inputId: "f_projectEdit", isEdit: true},
  {dlid: "list_sectionsEdit", inputId: "f_sectionEdit", isEdit: true},
];
const SUBVIEWS = [
  {id: "t_orders", displayStyle: "", viewKey: "main"},
  {id: "t_lineItems", displayStyle: "", viewKey: "lineItems"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editOrderOpen)},
];
const mainOpenInner = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const popOrderTable = popOrderTableWrapper(TBID);
const popLineItemTable = popLineItemTableWrapper(TBID2, false);
const confirmOrderDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this order and all associated line item entries? All data will be lost.`,
  deleteOrder);
const confirmToggleBilledStatus = confirmToggleBilledStatusWrapper(cbConfirmBilledStatusToggle);
const confirmToggleCompleteStatus = confirmToggleCompleteStatusWrapper(cbConfirmCompleteStatusToggle);

const [modal, hideModal, showModal] = setupModal("tobill_modal", "tobill_span");

function mainOpen() {
  mainOpenInner();
  currentOrder = null;
};

popSidebar("Orders");

// init
Promise.all([
  listOrders(),
])
  .then(() => {
    successAlert("Orders view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return listProjects();
  })
  .catch(errorAlert);

setFormListenerWrapper("f_edit_order", editOrder);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listOrders, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listOrders() {
  return API.listOrderSkeletons(searchQS, limitQS, pageQS)
    .then((os) => {
      popOrderTable(os);
      ordersMap = new Map(os.map((o) => [o.id, o]));
    })
    .catch(errorAlert);
};

function listProjects() {
  return API.listProjects()
    .then((ps) => {
      projectsMap = new Map(ps.map((p) => [p.name, p]));
      projectsIdMap = new Map(ps.map((p) => [p.id, p]));
      popListsInner([DATALISTS[0]], ps);
    })
    .catch(errorAlert);
};

function openLineItems(oid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  return API.getOrderById(oid)
    .then((o) => {
      setTableCaption("cap_lineItems", `Order #: ${o.number} &rarr; Line Items`);
      popLineItemTable(o.lineItems);
      currentOrder = o;
      return;
    })
    .catch(errorAlert);
};

function openOrderFlags(oid) {
  const o = ordersMap.get(oid);
  infoAlert(`
    Was wrapped: ${o.isWrapped ? "Yes" : "No"}<br />
  `);
  return;
};

function editOrderOpen(oid) {
  if (oid === null || oid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, oid]]);
  return fillEditOrder(oid);
};

function fillEditOrder(oid) {
  const o = ordersMap.get(oid);
  var fields = [
    {id: "f_orderNumberEdit", val: o.number},
    {id: "f_poEdit", val: o.po},
    {id: "f_orderTypeEdit", val: o.type},
    {id: "f_idEdit", val: o.id},
  ];
  if (o.projectId) {
    const mp = projectsIdMap.get(o.projectId);
    if (mp) {
      sectionsMap = new Map(mp.sections.map((s) => [s.name, s]));
      popListsInner([DATALISTS[1]], mp.sections);
      sectionsIdMap = new Map(mp.sections.map((s) => [s.id, s]));
      const ms = sectionsIdMap.get(o.sectionId);
      fields = fields.concat([
        {id: "f_projectEdit", val: mp.name},
        {id: "f_sectionEdit", val: ms.name},
      ]);
    }
  }
  fields.forEach(setInputVal);
  return;
};

function popSections(event) {
  event.preventDefault();
  event.stopPropagation();
  const n = document.getElementById("f_projectEdit");
  const mp = projectsMap.get(n.value);
  if (mp && mp.sections) {
    sectionsMap = new Map(mp.sections.map((s) => [s.name, s]));
    sectionsIdMap = new Map(mp.sections.map((s) => [s.id, s]));
    popListsInner([DATALISTS[1]], mp.sections);
    infoAlert("Sections list populated.");
  } else {
    errorAlert("Project and/or sections not found. Ensure one is selected first.");
  }
};

function editOrder(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const oid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(oid)) {
    errorAlert("Invalid order id provided. Contact system admin.");
    return;
  }
  const pname = fd.get("projectName");
  var pid = null;
  if (pname) {
    const mp = projectsMap.get(pname);
    if (mp) {
      pid = mp.id;
    }
  }
  const sname = fd.get("projectSection");
  var sid = null;
  if (sname) {
    const ms = sectionsMap.get(sname);
    if (ms) {
      sid = ms.id;
    }
  }
  return API.editOrder(oid, mkOrder(fd, oid), pid)
    .then(() => {
      infoAlert("Order edit complete.");
      if (fd.get("type") === "FP" && pid && sid) {
        return API.assignOrderToProject(oid, pid, sid)
          .then(() => {
            infoAlert("Order assigned to designated project.");
            event.target.reset();
            mainOpen();
            return listOrders();
          })
          .catch(errorAlert);
      }
      event.target.reset();
      mainOpen();
      return listOrders();
    })
    .catch(errorAlert);
};

function mkOrder(fd, oid, baseItem=null) {
  const baseOrder = ordersMap.get(oid);
  const baseFields = [
    {f: "number"},
    {f: "po"},
    {f: "type"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(baseOrder, baseItem, fd, baseFields, editFields);
};

function deleteOrder(oid) {
  return API.deleteOrder(oid)
    .then(() => {
      closeDialogs();
      successAlert("Order and line items deleted with required inventory adjustments.");
      return listOrders();
    })
    .catch((e) => {
      closeDialogs();
      return errorAlert(e);
    });
};

function writeLineItemReport() {
  if (!currentOrder || !currentOrder.id) {
    errorAlert("An order must be selected first.");
    return;
  }
  return API.getOrderLineItemDetails(currentOrder.id)
    .then(() => {
      successAlert("Line item report generated.");
      return;
    })
    .catch(errorAlert);
};

function genPackingSlip() {
  if (!currentOrder || !currentOrder.id) {
    errorAlert("An order must be selected first.");
    return;
  }
  return API.genOrderPackingSlip(currentOrder.id)
    .then(() => {
      successAlert("Packing slip generated.");
      return;
    })
    .catch(errorAlert);
};

function cbConfirmBilledStatusToggle(oid, isBilled) {
  return function inner() {
    closeDialogs();
    if (isBilled) {
      return API.setOrderToNotBilled(oid)
        .then(() => {
          successAlert("Order set to not billed.");
          return listOrders();
        })
        .catch(errorAlert);
    }
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
      return API.setOrderToBilled(oid, txt)
        .then(() => {
          closeDialogs();
          successAlert("Order set to billed.");
          mainOpen();
          return listOrders();
        })
        .catch(errorAlert);
    });
  };
};

function cbConfirmCompleteStatusToggle(oid, isComplete) {
  return function inner() {
    closeDialogs();
    if (isComplete) {
      return API.setOrderToNotComplete(oid)
        .then(() => {
          successAlert("Order set to not complete.");
          return listOrders();
        })
        .catch(errorAlert);
    }
    return API.setOrderToComplete(oid)
      .then(() => {
        closeDialogs();
        successAlert("Order set to complete.");
        mainOpen();
        return listOrders();
      })
      .catch(errorAlert);
  };
};

function manageNonCompleteNonBilledOrders(event) {
  event.preventDefault();
  event.stopPropagation();
  infoAlert("Retrieving list of non-project orders not yet complete or billed.");
  return API.getNonCompleteBilledOrders()
    .then((os) => {
      // returned as order number only! not objects
      closeDialogs();
      infoAlert("Checking app for latest order status...");
      return Promise.allSettled(os.map(API.checkAndUpdateOrderStatus))
        .then(() => {
          closeDialogs();
          successAlert("All order statuses updated. Ready to invoice complete orders.");
          return listOrders();
        })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

function listNonBilledOrders(event) {
  event.stopPropagation();
  event.preventDefault();
  return API.getNonBilledOrders()
    .then((os) => {
      return Promise.allSettled(os.map(API.checkOrderStatus))
        .then((jexts) => {
          showModal();
          const n = document.getElementById("tobill_body");
          var markdown = `<ol>`;
          for (const [idx, val] of jexts.entries()) {
            if (!val || !val.value) {
              markdown += `<li>${os[idx]}</li>`;
            } else {
              markdown += `<li>${os[idx]}<ul>
                <li>Complete: ${formatDateString(val.value.readyDate)}</li>
                <li>Shipped: ${formatDateString(val.value.shippedDate)}</li>
                <li>Ship Type: ${val.value.shipType}</li>
                </ul>`;
            }
          }
          markdown += `</ol>`;
          n.innerHTML = markdown;
          return;
        })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

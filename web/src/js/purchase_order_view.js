"use strict";

var matPurchaseOptionsMap = new Map();
var purchasesMap = new Map();
var materialsMap = new Map();
var currentPO;
var currentItem;
var currentMaterial;
var currentPOItem;
const POIDKEY = "poid";
const TBID = "t_items";
const FORM = "f_items";
const inps = ["f_name", "f_description", "f_balanceCategory"];
const DATALISTS = [
  {dlid: "list_purchase", inputId: "f_purchase", isEdit: false},
  {dlid: "list_material", inputId: "f_material", isEdit: false},
  {dlid: "list_matps", inputId: "f_matPurchase", isEdit: false},
];
const SUBVIEWS = [
  {id: "d_sub_po_items", displayStyle: "flex", viewKey: "main"}, // 0
];

const confirmClosePO = simpleDeleteConfirmationProcess(
  `Are you sure you want to close the PO? This cannot be undone.`,
  closePO,
  `PO not marked as closed.`);

const confirmReceivePO = simpleDeleteConfirmationProcess(
  `Are you sure you want to mark the PO as received? This will adjust inventory and cannot be undone.`,
  receivePO,
  `PO not marked as received.`);

const confirmInQB = simpleDeleteConfirmationProcess(
  `Are you sure you want to mark the PO as in QB? This cannot be undone.`,
  inQB,
  `PO not marked as in QB.`);

const confirmSendPO = simpleDeleteConfirmationProcess(
  `Are you sure you want to mark the PO as sent? This will place the inventory to "On order" and cannot be undone.`,
  sendPO,
  `PO not marked as sent.`);

const confirmPriceVerified = simpleDeleteConfirmationProcess(
  `Select yes if the prices on the PO have been verified against the invoice(s) and all adjustments have been made.`,
  priceVerifiedPO,
  `PO prices not marked as verified.`);

const confirmPriceAdjusted = simpleDeleteConfirmationProcess(
  `Select yes if the PO is both received and the prices are correct. This will merge pricing with the database and cannot be undone.`,
  priceAdjustedPO,
  `PO prices not merged with database.`);

const confirmRemoveItem = simpleDeleteConfirmationProcess(
  `Are you sure you want to remove this PO line item?`,
  removeItem,
  `Item not removed.`);

const confirmBackOrder = simpleDeleteConfirmationProcess(
  `Are you sure you want to generate a back order with the given numbers?`,
  createBackOrder,
  `Back order not generated.`);

popSidebar("");

// init
getPO()
  .then(() => {
    successAlert("Purchase order view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      popMaterials(),
      popPurchaseItems(),
    ]).catch(errorAlert);
  })
  .catch(errorAlert);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

function popPurchaseItems() {
  return API.listPurchaseItems()
    .then((ps) => {
      purchasesMap = new Map(ps.map((p) => [p.name, p]));
      popListsInner([DATALISTS[0]], ps);
    })
    .catch(errorAlert);
};

function popMaterials() {
  return API.listMaterials()
    .then((ms) => {
      materialsMap = new Map(ms.map((m) => [m.name, m]));
      popListsInner([DATALISTS[1]], ms);
    })
    .catch(errorAlert);
};

function getPO() {
  const mpoid = getQSId(POIDKEY, id => id)();
  return API.getPOById(mpoid)
    .then(fillInfo)
    .catch(errorAlert);
};

function fillInfo(po) {
  currentPO = po;
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  po.items.forEach((poi) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${poi.name}</td>
      <td>${renderBlankCol(poi.partNumber)}</td>
      <td>${poi.description}</td>
      <td>${poi.quantity}</td>
      ${renderNullTD(poi.unitsPerQuantity)}
      <td>$${formatPrice(poi.purchasePriceEach)}</td>
      <td>${poi.referenceType}</td>
      <td>${poi.balanceCategory}</td>
      <td>${poi.note}</td>
      <td>${renderBlankCol(poi.leadTime)}</td>
      <td><button class="table-button" onclick="editPOItemOpen(${poi.id});"><i class="fa fa-edit"</i></button></td>
      <td><button class="table-button" onclick="confirmRemoveItem(${poi.id});"><i class="fa fa-trash"</i></button></td>
      <td><input id="${poi.id}" type="number" min="0.00" max="${poi.quantity}" value="0" /></td>
    `;
    nextRow.innerHTML = markup;
  });
  return setTotalPrice(po);
};

function setTotalPrice(po) {
  const tp = po.items.reduce((acc, n) => acc + (n.purchasePriceEach * n.quantity), 0.00);
  setTableCaption("cap_items", `Purchase Order #${po.number} Items - ${po.vendor.name} &rarr; Current Total: <b>$${formatPrice(tp)}</b>`);
  return setButtons(po);
};

function setButtons(po) {
  const buttons = [
    { id: "button_inQB", preReqs: [], postReqs: ["isOrderInQB"] },
    { id: "button_sent", preReqs: ["isOrderInQB"], postReqs: ["isOrderSent"] },
    { id: "button_receive", preReqs: ["isOrderInQB", "isOrderSent"], postReqs: ["isOrderReceived"] },
    { id: "button_verify", preReqs: ["isOrderInQB", "isOrderSent"], postReqs: ["isPriceAdjusted"] },
    {id: "button_adjust", preReqs: ["isOrderInQB", "isOrderSent", "isPriceVerified", "isOrderReceived"], postReqs: ["isPriceAdjusted"]},
    {id: "button_close", preReqs: ["isOrderInQB", "isOrderSent", "isPriceVerified", "isOrderReceived", "isPriceAdjusted"], postReqs: ["isOrderClosed"]},
    {id: "input_backOrder", preReqs: ["isOrderInQB", "isOrderSent"], postReqs: ["isOrderReceived", "isPriceAdjusted", "isOrderClosed"], isBackOrder: true},
  ];
  buttons.forEach(({id, preReqs, postReqs, isBackOrder=false}) => {
    const n = document.getElementById(id);
    const preReqsMet = preReqs.every((p) => { return po[p]; });
    const postReqsMet = postReqs.some((p) => { return po[p]; });
    if (preReqsMet && !postReqsMet) {
      n.disabled = false;
      n.className = isBackOrder ? "toggle submit" : "toggle";
    } else {
      n.disabled = true;
      n.className = isBackOrder ? "toggle-disabled submit" : "toggle-disabled";
    }
  });
  return;
};

function setMatAsCurr(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(document.getElementById(FORM));
  const mmat = fd.get("material");
  if (mmat === "null" || mmat === undefined || mmat === "") {
    errorAlert("No material currently set.");
    return;
  }
  const mat = materialsMap.get(mmat);
  currentMaterial = mat;
  if (mat.name.startsWith("Generic")) {
    return setCurrentItem(event, currentMaterial, "costPerUnit");
  }
  return API.listMatPurchaseOptionsForMaterial(mat.id)
    .then((mps) => {
      matPurchaseOptionsMap = new Map(mps.map((mp) => [mp.name, mp]));
      popListsInner([DATALISTS[2]], mps);
      return;
    })
    .catch(errorAlert);
};

function setPurAsCurr(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(document.getElementById(FORM));
  const mpi = fd.get("purchase");
  if (mpi === null || mpi === undefined || mpi === "") {
    errorAlert("No purchase item currently set.");
    return;
  }
  const mi = purchasesMap.get(mpi);
  return setCurrentItem(event, mi, "cost");
};

function setMatPurchaseAsCurr(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(document.getElementById(FORM));
  const mmpo = fd.get("matPurchase");
  if (mmpo === null || mmpo === undefined || mmpo === "") {
    errorAlert("No material purchase option currently set.");
    return;
  }
  const mpo = matPurchaseOptionsMap.get(mmpo);
  if (mpo === undefined || mpo === null) {
    errorAlert("Material purchase option was not found or is not in the supported list.");
    return;
  }
  mpo.referenceType = "material purchase";
  mpo.balanceCategory = currentMaterial.balanceCategory;
  const fieldsToAdjust = [
    {id: "f_referenceId", prop: "id"},
    {id: "f_name", prop: "name"},
    {id: "f_referenceType", prop: "referenceType"},
    {id: "f_description", prop: "description"},
    {id: "f_balanceCategory", prop: "balanceCategory"},
    {id: "f_unitsPerQuantity", prop: "unitsPerQuantity"},
    {id: "f_leadTime", prop: "leadTime"},
  ];
  fieldsToAdjust.forEach(({id, prop, innerProp}) => {
    const inp = document.getElementById(id);
    if (innerProp) {
      inp.value = mpo[prop][innerProp];
    } else {
      inp.value = mpo[prop];
    }
  });
  var temp = inps;
  temp.push("f_unitsPerQuantity");
  temp.forEach((n) => {
    const i = document.getElementById(n);
    i.readOnly = true;
  });
};

function setCurrentItem(event, mi, propPrice) {
  if (mi === undefined || mi === null) {
    errorAlert("Item was not found or is not in the supported item list(s).");
    return;
  }
  currentItem = mi;
  const fieldsToAdjust = [
    {id: "f_referenceId", prop: "inventory", innerProp: "referenceId"},
    {id: "f_name", prop: "inventory", innerProp: "name"},
    {id: "f_referenceType", prop: "inventory", innerProp: "type"},
    {id: "f_description", prop: "description"},
    {id: "f_balanceCategory", prop: "balanceCategory"},
    {id: "f_purchasePriceEach", prop: propPrice},
    {id: "f_leadTime", prop: "leadTime"},
  ];
  fieldsToAdjust.forEach(({id, prop, innerProp}) => {
    const inp = document.getElementById(id);
    if (innerProp) {
      inp.value = mi[prop][innerProp];
    } else {
      inp.value = mi[prop];
    }
  });
  inps.forEach((n) => {
    const i = document.getElementById(n);
    i.readOnly = mi.isLocked;
  });
};

function addPOItem(event) {
  event.stopPropagation();
  event.preventDefault();
  const n = document.getElementById(FORM);
  const fd = new FormData(n);
  const baseItem = {
    id: 0,
    poId: currentPO.id,
    note: "",
    leadTime: null,
  };
  return API.addPOItem(currentPO.id, mkPOItem(fd, baseItem))
    .then(() => {
      successAlert("PO item added.");
      n.reset();
      return getPO();
    })
    .catch(errorAlert);
};

function mkPOItem(fd, baseItem=null) {
  const defObjShape = { id: 0, poId: currentPO.id, note: "" };
  const baseFields = [
    {f: "referenceId", parser: safeParseInt},
    {f: "referenceType"},
    {f: "name"},
    {f: "description"},
    {f: "balanceCategory"},
    {f: "quantity", parser: safeParseDouble},
    {f: "unitsPerQuantity", parser: safeParseMaybeDouble},
    {f: "purchasePriceEach", parser: safeParseDouble},
    {f: "note"},
    {f: "leadTime"},
  ];
  const editFields = [];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function addPOItemPurchase(event) {
  return addPOItem("purchase", "Purchase", purchasesMap)(event);
};

function addPOItemMaterial(event) {
  return addPOItem("material", "Material", materialsMap)(event);
};

function closePO(event) {
  event.stopPropagation();
  event.preventDefault();
  return API.setAsClosed(currentPO.id)
    .then(() => {
      successAlert("PO marked as closed.");
      return getPO();
    })
    .catch(errorAlert);
};

function receivePO(event) {
  event.stopPropagation();
  event.preventDefault();
  infoAlert("This may take a moment - may need to send an email notification. Please be patient.");
  return API.setAsReceived(currentPO.id)
    .then(() => {
      closeDialogs();
      successAlert("PO marked as received. Inventory adjustments should be complete. Please verify.");
      return getPO();
    })
    .catch(errorAlert);
};

function sendPO(event) {
  event.stopPropagation();
  event.preventDefault();
  return API.setAsSent(currentPO.id)
    .then(() => {
      successAlert("PO marked as sent. Inventory adjustments placed to on order. Please verify.");
      return getPO();
    })
    .catch(errorAlert);
};

function inQB(event) {
  event.stopPropagation();
  event.preventDefault();
  return API.setQBDone(currentPO.id)
    .then(() => {
      successAlert("PO marked as in QB.");
      return getPO();
    })
    .catch(errorAlert);
};

function removeItem(poiid) {
  return API.removePOItem(currentPO.id, poiid)
    .then(() => {
      successAlert("PO item removed.");
      return getPO();
    })
    .catch(errorAlert);
};

function editPOItemOpen(poiid) {
  const poi = (currentPO.items.filter((i) => i.id === poiid))[0];
  currentPOItem = poi;
  const fieldsToAdjust = [
    {id: "f_referenceId", prop: "referenceId"},
    {id: "f_name", prop: "name"},
    {id: "f_referenceType", prop: "referenceType"},
    {id: "f_description", prop: "description"},
    {id: "f_balanceCategory", prop: "balanceCategory"},
    {id: "f_unitsPerQuantity", prop: "unitsPerQuantity"},
    {id: "f_leadTime", prop: "leadTime"},
    {id: "f_purchasePriceEach", prop: "purchasePriceEach"},
    {id: "f_note", prop: "note"},
    {id: "f_quantity", prop: "quantity"},
  ];
  fieldsToAdjust.forEach(({id, prop, innerProp}) => {
    const inp = document.getElementById(id);
    if (innerProp) {
      inp.value = poi[prop][innerProp];
    } else {
      inp.value = poi[prop];
    }
  });
  inps.forEach((n) => {
    const i = document.getElementById(n);
    i.readOnly = poi.isLocked;
  });
  const btn = document.getElementById("but_submit");
  btn.value = "(2): Edit PO line item";
  btn.onclick = editPOItem;
};

function editPOItem(event) {
  event.stopPropagation();
  event.preventDefault();
  const n = document.getElementById(FORM);
  const fd = new FormData(n);
  return API.editPOItem(currentPO.id, currentPOItem.id, mkPOItem(fd, currentPOItem))
    .then(() => {
      successAlert("PO item edited.");
      currentPOItem = null;
      const btn = document.getElementById("but_submit");
      btn.value = "(2): Add PO line item";
      btn.onclick = addPOItem;
      n.reset();
      return getPO();
    })
    .catch(errorAlert);
};

function getPORfq() {
  return API.getPORfq(currentPO.id)
    .then(() => {
      successAlert("PO RFQ generation successful.");
      return;
    })
    .catch(errorAlert);
};

function getPOReport() {
  return API.getPOReport(currentPO.id)
    .then(() => {
      successAlert("PO report generation successful.");
      return;
    })
    .catch(errorAlert);
};

function createBackOrder() {
  var pois = [];
  var errString = null;
  currentPO.items.forEach((poi) => {
    const n = document.getElementById(`${poi.id}`);
    if (!n) {
      return;
    }
    const [es, sd]  = safeParseDouble(n.value);
    if (es !== null) {
      errString = es;
    }
    if (sd > poi.quantity) {
      errString = "Invalid quantity - more than originally ordered.";
    }
    if (sd < 0) {
      errString = "Invalid quantity - must be greater than zero.";
    }
    if (sd >= 0.1) {
      pois.push({"id": poi.id, "quantity": sd});
    }
  });
  if (errString) {
    errorAlert(errString);
    return;
  }
  if (pois.length === 0) {
    errorAlert("No purchase items were marked as back order.");
    return;
  }
  return API.createBackOrder(currentPO.id, pois)
    .then((msg) => {
      successAlert(msg.msg);
      return getPO();
    })
    .catch(errorAlert);
};

function priceVerifiedPO(event) {
  event.preventDefault();
  event.stopPropagation();
  infoAlert("This may take a moment - may need to send an email notification. Please be patient.");
  return API.setPOPriceVerified(currentPO.id)
    .then((msg) => {
      closeDialogs();
      successAlert(msg.msg);
      return getPO();
    })
    .catch(errorAlert);
};

function priceAdjustedPO(event) {
  event.preventDefault();
  event.stopPropagation();
  return API.setPOPriceAdjusted(currentPO.id)
    .then((msg) => {
      successAlert(msg.msg);
      return getPO();
    })
    .catch(errorAlert);
};

"use strict";

var materialsMap = new Map();
var currentQuote;
var currentCurb;
var curbsMap = new Map();
const IDKEY = "pid";
const IDKEY2 = "cid";
const TBID = "t_curbs";
const SUBVIEWS = [
  {id: "t_curbs", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY2, editCurbOpen)},
  {id: "d_sub_order_form", displayStyle: "flex", viewKey: "order"},
];
const DATALISTS = [
  {dlid: "metal_list", inputId: "f_metal", isEdit: false},
  {dlid: "metal_listEdit", inputId: "f_metalEdit", isEdit: true},
];

const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY2]);
const confirmDeleteCurb = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this curb? All data will be lost.`,
  deleteCurb);

popSidebar();

// init
Promise.all([
  getQuote(),
])
  .then(() => {
    populateMaterialList();
    successAlert("CC quote view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper("f_curb", addCurb);
setFormListenerWrapper("f_edit_curb", editCurb);
setFormListenerWrapper("f_quote_to_order", createOrderFromQuote);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function populateMaterialList() {
  return API.listMaterials()
    .then((ms) => {
      materialsMap = new Map(ms.map((m) => [m.name, m]));
      popListsInner(DATALISTS, ms);
      setupMetalsDatalists("f_metalId")(DATALISTS, materialsMap);
    })
    .catch(errorAlert);
};

function setupMetalsDatalists(idname) {
  return setupDatalistsGeneric(
    [
      {id: idname, prop: "id"},
    ], (val) => `Chosen material ${val} is not in the supported materials list. Please add this material first.`);
};

function getQuote() {
  const mpid = getQSId(IDKEY, id => id)();
  if (!mpid) {
    errorAlert("Quote PID query parameter not set. Re-select quote from main CC view.");
    currentQuote = null;
    return;
  }
  return API.getCCQuoteByPID(mpid)
    .then(popTable)
    .catch((err) => { return Promise.reject(err); });
};

function popTable(q) {
  currentQuote = q;
  setTableCaption("cap_curbs", `CC quote #AT-${q.pid}`);
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  q.curbs.forEach((c) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${c.id}</td>
      <td>${c.quantity}</td>
      <td>${c.size}</td>
      <td>${c.oldUnit}</td>
      <td>${c.newUnit}</td>
      <td>${c.adapter}</td>
      <td>$${formatPrice(c.priceEach)}</td>
      <td>${c.metalName}</td>
      <td>${c.metalWeight}</td>
      <td>${c.gasketFeet}</td>
      <td><button class="table-button" onclick="editCurbOpen(${c.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmDeleteCurb(${c.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  curbsMap = new Map(q.curbs.map((c) => [c.id, c]));
  return;
};

function openAddCurb(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  return;
};

function addCurb(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
  };
  return API.addCurbToQuote(currentQuote.pid, mkCurb(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New curb added.");
      mainOpen();
      return getQuote();
    })
    .catch(errorAlert);
};

function editCurbOpen(cid) {
  if (cid === null || cid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  return fillEditCurb(cid);
};

function fillEditCurb(cid) {
  console.log(cid);
  console.log(curbsMap);
  const c = curbsMap.get(cid);
  [ {id: "f_quantityEdit", val: c.quantity},
    {id: "f_idEdit", val: c.id},
    {id: "f_oldUnitEdit", val: c.oldUnit},
    {id: "f_newUnitEdit", val: c.newUnit},
    {id: "f_adapterEdit", val: c.adapter},
    {id: "f_basePriceEachEdit", val: c.basePriceEach},
    {id: "f_sizeEdit", val: c.size},
    {id: "f_metalIdEdit", val: c.metalId},
    {id: "f_metalEdit", val: c.metalName},
    {id: "f_gasketFeetEdit", val: c.gasketFeet},
    {id: "f_metalWeightEdit", val: c.metalWeight},
  ].forEach(setInputVal);
  currentCurb = c;
  return;
};

function editCurb(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const cid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(cid)) {
    errorAlert("Invalid curb id provided. Contact system admin.");
    return;
  }
  return API.editCurbOnQuote(currentQuote.pid, cid, mkCurb(fd))
    .then(() => {
      successAlert("Curb edit complete.");
      event.target.reset();
      mainOpen();
      return getQuote();
    })
    .catch(errorAlert);
};

function mkCurb(fd, baseItem=null) {
  const defObjShape = { };
  const baseFields = [
    {f: "oldUnit"},
    {f: "newUnit"},
    {f: "adapter"},
    {f: "size"},
    {f: "basePriceEach", parser: safeParseDouble},
    {f: "quantity", parser: safeParseInt},
    {f: "metalId", parser: safeParseInt},
    {f: "metalWeight", parser: safeParseDouble},
    {f: "gasketFeet", parser: safeParseDouble},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deleteCurb(cid) {
  return API.deleteCurbFromQuote(currentQuote.pid, cid)
    .then(() => {
      closeDialogs();
      successAlert("Curb deletion successful.");
      return getQuote();
    })
    .catch(errorAlert);
};

function genReport(event) {
  event.stopPropagation();
  event.preventDefault();
  return API.getCCQuoteHTMLReport(currentQuote.pid)
    .then(() => {
      successAlert("CC quote generation successful.");
    })
    .catch(errorAlert);
};

function openToOrder(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  setTableCaption("cap_toOrder", "Convert to Order");
  return;
};

function createOrderFromQuote(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const mordNum = fd.get("ordNumber");
  const mordPO = fd.get("ordPo");
  if (!mordNum) {
    errorAlert("Order number must be set.");
    return;
  }
  if (!mordPO) {
    errorAlert("Order PO must be set.");
    return;
  }
  return API.convertCCQuoteToOrder(currentQuote.pid, mordNum, mordPO)
    .then(() => {
      successAlert("Order creation successful.");
      event.target.reset();
      mainOpen();
      return;
    })
    .catch(errorAlert);
};

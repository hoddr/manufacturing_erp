"use strict";

var purchaseOrdersMap = new Map();
var vendorsMap = new Map();
var currentPO;
var showClosedFlag = false;
var showIsBackOrder = true;
[searchQS, limitQS, pageQS] = getPaginationQS("25"); // 25 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const POIDKEY = "poid";
const TBID = "t_purchaseOrders";
const ADD_FORM = "f_purchase_order";
const EDIT_FORM = "f_purchase_order_edit";
const DATALISTS = [
  {dlid: "list_item_vendors", inputId: "f_vendor", isEdit: false},
  {dlid: "list_item_vendorsEdit", inputId: "f_vendorEdit", isEdit: true}
];
const SUBVIEWS = [
  {id: TBID, displayStyle: "", viewKey: "main", fn: () => { currentPO = null; }}, // 0
  {id: "d_sub_poAdd_form", displayStyle: "flex", viewKey: "add"}, // 1
  {id: "d_sub_poEdit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(POIDKEY, editPOOpen)}, // 2
];

const confirmDeletePO = simpleDeleteConfirmationProcess(
  `Are you sure you wish to delete this purchase order? All data will be lost.`,
  deletePO);
const mainOpenInner = mainOpenWrapper(SUBVIEWS, [POIDKEY]);
const mainOpen = () => {
  mainOpenInner();
  currentPO = null;
};

popSidebar("Purchase Orders");

// init
listPOs()
  .then(() => {
    successAlert("Purchase orders view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return popVendorList();
  })
  .catch(errorAlert);

setFormListenerWrapper(EDIT_FORM, editPO);
setFormListenerWrapper(ADD_FORM, addPO);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listPOs, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

function listPOs() {
  const fn = showClosedFlag ? API.listClosedPOs : API.listPOs;
  return fn(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

// TODO repeated in many files...
function popVendorList() {
  return API.listVendors()
    .then((vs) => {
      vendorsMap = new Map(vs.map((v) => [v.name, v]));
      setupVendorDatalists(DATALISTS, vendorsMap);
      popListsInner(DATALISTS, vs);
      return;
    })
    .catch(errorAlert);
};

function popTable(pos) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  pos
    .filter((po) => (!po.isBackOrder || showIsBackOrder) &&  (showClosedFlag || !po.isOrderClosed))
    .forEach((po) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td><a href="<<BASE_URL>>/views/purchase_order_view.html?poid=${po.id}">${po.number}</a></td>
      <td>${po.vendor.name}</td>
      ${renderDateTD(po.expectedReceiptDate)}
      ${renderDateTD(po.receiptDate)}
      ${renderBoolTD(po.isOrderInQB)}
      ${renderBoolTD(po.isOrderSent)}
      ${renderBoolTD(po.isOrderReceived)}
      ${renderBoolTD(po.isPriceVerified)}
      ${renderBoolTD(po.isPriceAdjusted)}
      ${renderBoolTD(po.isOrderClosed)}
      ${renderBoolTD(po.isBackOrder)}
      <td><button class="table-button" onclick="editPOOpen(${po.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmDeletePO(${po.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  purchaseOrdersMap = new Map(pos.map((po) => [po.id, po]));
  return;
};

function addPOOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addPO(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    number: "",
    isOrderInQB: false,
    isOrderSent: false,
    isOrderReceived: false,
    isOrderClosed: false,
    isBackOrder: false,
    isPriceAdjusted: false,
    isPriceVerified: false,
    items: [],
    vendor: {
      id: 0, // default placeholder
    },
  };
  return API.addPO(mkPO(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New purchase order created.");
      mainOpen();
      return listPOs();
    })
    .catch(errorAlert);
};

function editPOOpen(poid) {
  if (poid === null || poid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[POIDKEY, poid]]);
  return fillEditPO(poid);
};

function fillEditPO(poid) {
  const po = purchaseOrdersMap.get(poid);
  [ {id: "f_vendorEdit", val: po.vendor.name},
    {id: "f_expectedReceiptDateEdit", val: (new Date(po.expectedReceiptDate)).toISOString().slice(0,10)},
    {id: "f_memoEdit", val: po.memo},
    {id: "f_idEdit", val: po.id},
    {id: "f_vendorIdEdit", val: po.vendor.id},
    {id: "f_vendorNameEdit", val: po.vendor.name},
    {id: "f_vendorCompanyEdit", val: po.vendor.company},
    {id: "f_numberEdit", val: po.number},
  ].forEach(setInputVal);
  return;
};

function editPO(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const poid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(poid)) {
    errorAlert("Invalid purchase order id provided. Contact system admin.");
    return;
  }
  const mpo = purchaseOrdersMap.get(poid);
  if (mpo === null || mpo === undefined) {
    errorAlert("Existing purchase order with given id not found. Reload page.");
    return;
  }
  return API.editPO(poid, mkPO(fd, null, mpo))
    .then(() => {
      event.target.reset();
      successAlert("Purchase order edit complete.");
      mainOpen();
      return listPOs();
    })
    .catch(errorAlert);
};

function mkPO(fd, baseItem=null, editPO=null) {
  const defObjShape = editPO;
  const intField = "vendor";
  const baseFields = [
    {f: "memo"},
    {f: "vendorId", actual: "id", parser: safeParseInt, isNested: true, intField: "vendor"},
    {f: "vendorName", actual: "name", isNested: true, intField: "vendor"},
    {f: "vendorCompany", actual: "company", isNested: true, intField: "vendor"},
    {f: "vendorCategory"},
    {f: "expectedReceiptDate", parser: safeParseDate},
    {f: "number"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deletePO(poid) {
  return API.deletePO(poid)
    .then(() => {
      successAlert("Purchase order deleted.");
      return listPOs();
    })
    .catch(errorAlert);
};

function toggleShowClosedFlag(event) {
  event.stopPropagation();
  event.preventDefault();
  showClosedFlag = !showClosedFlag;
  return listPOs();
};

function toggleShowBackOrderFlag(event) {
  event.stopPropagation();
  event.preventDefault();
  showIsBackOrder = !showIsBackOrder;
  return listPOs();
};

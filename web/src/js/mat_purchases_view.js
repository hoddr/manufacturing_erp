"use strict";

var materialPurchasesMap = new Map();
var materialsMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "matpid";
const TBID = "t_matps"
const EDIT_FORM = "f_edit_matp";
const ADD_FORM = "f_add_matp";
const DATALISTS = [
  {dlid: "list_mats", inputId: "f_material", isEdit: false},
  {dlid: "list_matsEdit", inputId: "f_materialEdit", isEdit: true},
];
const SUBVIEWS = [
  {id: "t_matps", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editMatPurchaseOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const confirmDelMatPurchase = simpleDeleteConfirmationProcess(
  `Are you sure wish to delete this material purchase option? It will no longer be available for purchase orders.`,
  deleteMatPurchase,
  `Material purchase option not deleted.`);

popSidebar("Material Purchases");

// init
Promise.all([
  listMatPurchases(),
])
  .then(() => {
    successAlert("Material purchase options view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      listMaterials(),
    ]).catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addMatPurchase);
setFormListenerWrapper(EDIT_FORM, editMatPurchase);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listMatPurchases, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listMatPurchases() {
  return API.listMatPurchases(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function listMaterials() {
  return API.listMaterials()
    .then((ms) => {
      materialsMap = new Map(ms.map((m) => [m.name, m]));
      popListsInner(DATALISTS, ms);
      setupMaterialDatalists(DATALISTS, materialsMap);
      return;
    })
    .catch(errorAlert);
};

function popTable(mps) {
  const tb = getTableBody(TBID);
  setTableCaption("cap_matps", "Material Purchase Options Master List");
  tb.innerHTML = "";
  mps.forEach((mp) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${mp.name}</td>
      <td>${mp.description}</td>
      <td>${mp.fkMaterialName}</td>
      <td>${mp.unitsPerQuantity}</td>
      <td>${renderBlankCol(mp.leadTime)}</td>
      <td><button class="table-button" onclick="editMatPurchaseOpen(${mp.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmDelMatPurchase(${mp.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  materialPurchasesMap = new Map(mps.map((mp) => [mp.id, mp]));
  return;
};

function addMatPurchaseOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addMatPurchase(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseMatPurchase = {
    id: 0, // default placeholder
    leadTime: null,
  };
  return API.addMatPurchase(mkMatPurchase(fd, baseMatPurchase))
    .then(() => {
      event.target.reset();
      successAlert("New material purchase added.");
      mainOpen();
      return listMatPurchases();
    });
};

function editMatPurchaseOpen(mpid) {
  if (mpid === null || mpid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, mpid]]);
  return fillEditMatPurchase(mpid);
};

function fillEditMatPurchase(mpid) {
  const mp = materialPurchasesMap.get(mpid);
  [ {id: "f_nameEdit", val: mp.name},
    {id: "f_descriptionEdit", val: mp.description},
    {id: "f_materialEdit", val: mp.fkMaterialName},
    {id: "f_materialIdEdit", val: mp.fkMaterialId},
    {id: "f_unitsQuantEdit", val: mp.unitsPerQuantity},
    {id: "f_leadTimeEdit", val: mp.leadTime},
    {id: "f_idEdit", val: mp.id},
  ].forEach(setInputVal);
};

function editMatPurchase(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const mid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(mid)) {
    errorAlert("Invalid material purchase option id provided. Contact system admin.");
    return;
  }
  return API.editMatPurchase(mid, mkMatPurchase(fd))
    .then(() => {
      event.target.reset();
      successAlert("Material purchase option edit complete.");
      mainOpen();
      return listMatPurchases();
    })
    .catch(errorAlert);
};

function mkMatPurchase(fd, baseMatPurchase=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "name"},
    {f: "description"},
    {f: "unitsPerQuantity", parser: safeParseDouble},
    {f: "material", actual: "fkMaterialName"},
    {f: "materialId", actual: "fkMaterialId", parser: safeParseInt},
    {f: "leadTime"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseMatPurchase, fd, baseFields, editFields);
};

function deleteMatPurchase(mpid) {
  return API.deleteMatPurchase(mpid)
    .then(() => {
      successAlert("Material purchase option deleted.");
      return listMatPurchases();
    })
    .catch(errorAlert);
};

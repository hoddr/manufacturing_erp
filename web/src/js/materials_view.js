"use strict";

var matsToReconcile = [];
var materialsMap = new Map();
var vendorsMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("100"); // 100 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "matid";
const TBID = "t_materials";
const EDIT_FORM = "f_edit_material";
const ADD_FORM = "f_add_material";
const DATALISTS = [
  {dlid: "list_vendors", inputId: "f_vendor", isEdit: false},
  {dlid: "list_vendorsEdit", inputId: "f_vendorEdit", isEdit: true}
];
const SUBVIEWS = [
  {id: "t_materials", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editMaterialOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);

popSidebar("Materials");

// init
Promise.all([
  listMaterials(),
])
  .then(() => {
    successAlert("Materials view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      populateVendorList(),
    ]).catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addMaterial);
setFormListenerWrapper(EDIT_FORM, editMaterial);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listMaterials, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listMaterials() {
  return API.listMaterials(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function populateVendorList() {
  return API.listVendors()
    .then((vs) => {
      vendorsMap = new Map(vs.map((v) => [v.name, v]));
      popListsInner(DATALISTS, vs);
      return;
    })
    .catch(errorAlert);
};

function popTable(ms) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  ms.forEach((m) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${m.name}</td>
      <td>${m.description}</td>
      <td>${m.balanceCategory}</td>
      <td>${m.unit}</td>
      <td>${renderBlankCol(m.surfaceDensity)}</td>
      <td>$${m.costPerUnit}</td>
      <td>${m.inventory.onHand}</td>
      <td>${m.inventory.onOrder}</td>
      ${renderNullTD(m.inventory.minOnHand)}
      <td>${renderBlankCol(m.preferredVendor)}</td>
      <td onclick="editMaterialOpen(${m.id});"><i class="fa fa-pencil"></i></td>
    `;
    nextRow.innerHTML = markup;
  });
  materialsMap = new Map(ms.map((m) => [m.id, m]));
  return;
};

function addMaterialOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addMaterial(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseMat = {
    inventory: {
      id: 0, // default placeholder
      referenceId: 0, // default placeholer
      type: "material",
    },
    isLocked: true,
    vendor: {
      id: 0, // default placeholder
    },
  };

  return API.addMaterial(mkMaterial(fd, baseMat))
    .then(() => {
      event.target.reset();
      successAlert("New material added.");
      mainOpen();
      return listMaterials();
    })
    .catch(errorAlert);
};

function editMaterialOpen(mid) {
  if (mid === null || mid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, mid]]);
  return fillEditMaterial(mid);
};

function fillEditMaterial(mid) {
  const m = materialsMap.get(mid);
  [ {id: "f_nameEdit", val: m.name},
    {id: "f_descriptionEdit", val: m.description},
    {id: "f_balanceCategoryEdit", val: m.balanceCategory},
    {id: "f_unitEdit", val: m.unit},
    {id: "f_surfaceDensityEdit", val: m.surfaceDensity},
    {id: "f_costEdit", val: m.costPerUnit},
    {id: "f_invIdEdit", val: m.inventory.id},
    {id: "f_invRefType", val: m.inventory.type},
    {id: "f_onHandEdit", val: m.inventory.onHand},
    {id: "f_onOrderEdit", val: m.inventory.onOrder},
    {id: "f_minOnHandEdit", val: m.inventory.minOnHand},
    {id: "f_preferredVendorEdit", val: m.preferredVendor},
    {id: "f_idEdit", val: m.id},
  ].forEach(setInputVal);
};

function editMaterial(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const mid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(mid)) {
    errorAlert("Invalid material id provided. Contact system admin.");
    return;
  }
  return API.editMaterial(mid, mkMaterial(fd))
    .then(() => {
      event.target.reset();
      successAlert("Material edit complete.");
      mainOpen();
      return listMaterials();
    })
    .catch(errorAlert);
};

function mkMaterial(fd, baseMat=null) {
  const defObjShape = { inventory: {}, isLocked: true };
  const intField = "inventory";
  const baseFields = [
    {f: "name"},
    {f: "description"},
    {f: "balanceCategory"},
    {f: "unit"},
    {f: "surfaceDensity", parser: safeParseMaybeDouble},
    {f: "costPerUnit", parser: safeParseDouble},
    {f: "preferredVendor"},
    {f: "onHand", parser: safeParseDouble, isNested: true, intField},
    {f: "onOrder", parser: safeParseDouble, isNested: true, intField},
    {f: "minOnHand", parser: safeParseDouble, isNested: true, intField},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
    {f: "invId", actual: "id", parser: safeParseInt, isNested: true, intField},
    {f: "id", actual: "referenceId", parser: safeParseInt, isNested: true, intField},
    {f: "type", isNested: true, intField},
  ];
  return mkObj(defObjShape, baseMat, fd, baseFields, editFields);
};

function invReconcile(event) {
  event.preventDefault();
  event.stopPropagation();
  if (Array.isArray(matsToReconcile) && matsToReconcile.length > 0) {
    return reconcilePrompt();
  }
  infoAlert("Retrieving full list of materials...");
  return API.listMaterials()
    .then((ms) => {
      const sortedMs = ms.sort((a, b) => a.inventory.id <= b.inventory.id);
      closeDialogs();
      successAlert("Materials retrieved.");
      matsToReconcile = ms;
      return reconcilePrompt();
    })
    .catch((e) => {
      closeDialogs();
      errorAlert(e);
      return;
    });
};

function reconcilePrompt() {
  closeDialogs();
  if (!Array.isArray(matsToReconcile) || matsToReconcile.length === 0) {
    matsToReconcile = [];
    infoAlert("All materials reconciled.");
    return;
  }
  const nextm = matsToReconcile.pop();
  return promptDialog(`Please enter the hand-counted inventory difference for material: ${nextm.name}. CAUTION: For a counted weight LOWER than the ERP, enter a POSTIVE value. For a counted weight HIGHER than the ERP, enter a NEGATIVE value, e.g. -1234.`,
    cbReconcile(nextm));
};

function cbReconcile(m) {
  return function(event) {
    event.preventDefault();
    event.stopPropagation();
    closeDialogs();
    const fd = new FormData(event.target);
    event.target.reset();
    const txt = fd.get("prompt");
    if (txt === "") {
      return infoAlert(`Invalid entry: "${txt}". A valid number must be entered.`);
    }
    const [errString, diffVal] = safeParseInt(txt);
    if (errString !== null) {
      errorAlert(`Parse error for inventory number: ${errString}.`);
      return;
    }
    infoAlert(`The update amount to be set is: ${m.inventory.onHand - diffVal}.`);
    m.inventory.onHand -= diffVal;
    return API.editInventory(m.inventory.id, m.inventory)
      .then(() => {
        closeDialogs();
        return reconcilePrompt();
      })
      .catch(errorAlert);
  };
};

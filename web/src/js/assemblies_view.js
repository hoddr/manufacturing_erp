"use strict";

var subItemsMap = new Map();
var currentAssembly;
var assembliesMap = new Map();
var purchasesMap = new Map();
var fabricationsMap = new Map();
var materialsMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("25"); // 25 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const TBID = "t_assemblies";
const TBIDSUB = "t_subItems";
const AIDKEY = "aid";
const ADD_FORM = "f_assembly";
const EDIT_FORM = "f_assembly_edit";
const SUB_FORM = "f_subItems";
const DATALISTS = [
  {dlid: "list_fabrication", inputId: "f_fabrication", isEdit: false},
  {dlid: "list_purchase", inputId: "f_purchase", isEdit: false},
  {dlid: "list_materialSub", inputId: "f_materialSub", isEdit: false},
];
const SUBVIEWS = [
  {id: "t_assemblies", displayStyle: "", viewKey: "main", fn: () => { currentAssembly = null; }}, // 0
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"}, // 1
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(AIDKEY, editAssemblyOpen)}, // 2
  {id: "d_sub_items_form", displayStyle: "flex", viewKey: "subItems", fn: getQSId(AIDKEY, openSubItems)}, // 3
  {id: "t_subItems", displayStyle: "", viewKey: "subItems"}, // 4
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [AIDKEY]);

const confirmDelete = simpleDeleteConfirmationProcess(
  `Are you sure you wish to delete this assembly? All data will be lost.`,
  deleteAssembly);
const confirmDelSubItem = simpleDeleteConfirmationProcess(
  `Are you sure you wish to remove this sub item?.`,
  deleteSubItem);

popSidebar("Assemblies");

// init
Promise.all([
  listAssemblies(),
  ])
  .then(() => {
    successAlert("Assemblies view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      populatePurchaseList(),
      populateFabricationList(),
      populateMaterialList(),
    ]).catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(EDIT_FORM, editAssembly);
setFormListenerWrapper(ADD_FORM, addAssembly);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listAssemblies, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function populateFabricationList() {
  return API.listFabItems()
    .then((fs) => {
      fabricationsMap = new Map(fs.map((f) => [f.name, f]));
      popListsInner([DATALISTS[0]], fs);
    })
    .catch(errorAlert);
};

function populatePurchaseList() {
  return API.listPurchaseItems()
    .then((ps) => {
      purchasesMap = new Map(ps.map((p) => [p.name, p]));
      popListsInner([DATALISTS[1]], ps);
    })
    .catch(errorAlert);
};

function populateMaterialList() {
  return API.listMaterials()
    .then((ms) => {
      materialsMap = new Map(ms.map((m) => [m.name, m]));
      popListsInner([DATALISTS[2]], ms);
      setupMaterialDatalists([DATALISTS[2]], materialsMap);
    })
    .catch(errorAlert);
};

function listAssemblies() {
  return API.listAssemblies(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function popTable(as) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  as.forEach((a) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${a.name}</td>
      <td>${a.description}</td>
      <td>${a.labor}</td>
      <td>$${formatPrice(a.laborPrice)}</td>
      <td>${a.inventory.onHand}</td>
      <td>${a.inventory.onOrder}</td>
      <td>${a.inventory.minOnHand}</td>
      <td><button class="table-button" onclick="openSubItems(${a.id});">(&rarr;) View sub items</button></td>
      <td><button class="table-button" onclick="editAssemblyOpen(${a.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmDelete(${a.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  assembliesMap = new Map(as.map((a) => [a.id, a]));
  return;
};

function addAssemblyOpen(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  subItemsMap = new Map();
  return;
};

function addAssembly(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0,
    laborPrice: 0.00,
    weight: 0.00,
    subItems: [],
    inventory: {
      id: 0, // default placeholder
      referenceId: 0, // default placeholder
      type: "assembly",
    },
  };
  return API.addAssembly(mkAssembly(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New assembly added.");
      mainOpen();
      return listAssemblies();
    })
    .catch(errorAlert);
};

function editAssemblyOpen(aid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[AIDKEY, aid]]);
  return fillEditAssembly(aid);
};

function fillEditAssembly(aid) {
  const a = assembliesMap.get(aid);
  [ {id: "f_nameEdit", val: a.name},
    {id: "f_descriptionEdit", val: a.description},
    {id: "f_idEdit", val: a.id},
    {id: "f_laborEdit", val: a.labor},
    {id: "f_invIdEdit", val: a.inventory.id},
    {id: "f_invRefTypeEdit", val: a.inventory.type},
    {id: "f_onHandEdit", val: a.inventory.onHand},
    {id: "f_onOrderEdit", val: a.inventory.onOrder},
    {id: "f_minOnHandEdit", val: a.inventory.minOnHand},
  ].forEach(setInputVal);
  currentAssembly = a;
  return;
};

function editAssembly(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const aid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(aid)) {
    errorAlert("Invalid assembly id provided. Contact system admin.");
    return;
  }
  return API.editAssemblyPartial(aid, mkAssembly(fd))
    .then(() => {
      event.target.reset();
      successAlert("Assembly edit complete.");
      mainOpen();
      return listAssemblies();
    })
    .catch(errorAlert);
};

function mkAssembly(fd, baseItem=null) {
  const defObjShape = {
    subItems: [],
    laborPrice: 0.00,
    weight: 0.00,
    inventory: {},
  };
  const intField = "inventory";
  const baseFields = [
    {f: "name"},
    {f: "description"},
    {f: "labor", parser: safeParseDouble},
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
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deleteAssembly(aid) {
  return API.deleteAssembly(aid)
    .then(() => {
      infoAlert("Assembly deletion successful.");
      return listAssemblies();
    })
    .catch(errorAlert);
};

function openSubItems(aid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  show(SUBVIEWS[4]);
  display(SUBVIEWS[3]);
  display(SUBVIEWS[4]);
  setQueryString([[AIDKEY, aid]]);
  return API.getAssemblyById(aid)
    .then(populateSubItemsTable)
    .catch(errorAlert);
};

function populateSubItemsTable(a) {
  currentAssembly = a;
  setTableCaption("cap_sub_items", `Assembly ${a.name} &rarr; Sub Items &rarr; Full price (with markup): $${a.price}`);
  const tb = getTableBody(TBIDSUB);
  tb.innerHTML = "";
  a.subItems.forEach((si) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${si.id}</td>
      <td>${si.inventory.name}</td>
      <td>${si.quantity}</td>
      <td>$${formatPrice(si.priceEach)}</td>
      <td>${si.inventory.type}</td>
      <td><button class="table-button" onclick="confirmDelSubItem(${si.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  subItemsMap = new Map(a.subItems.map((si) => [si.id, si]));
};

function deleteSubItem(sid) {
  return API.deleteSubItem(currentAssembly.id, sid)
    .then(() => {
      infoAlert("Sub item deleted.");
      return listAssemblies()
        .then(() => { openSubItems(currentAssembly.id) })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

function addSubItem(ff, ffquant, en, map) {
  return function addSubInner(event) {
    event.stopPropagation();
    event.preventDefault();
    const f = document.getElementById(SUB_FORM);
    const fd = new FormData(f);
    const name = fd.get(ff);
    const mq = fd.get(ffquant);
    const [errString, q] = safeParseDouble(mq);
    if (errString !== null) {
      errorAlert(errString);
      return;
    }
    const my = map.get(name);
    if (my === null || my === undefined) {
      errorAlert(`${en} item: ${name} not found.`);
      f.reset();
      return;
    }
    const baseItem = {
      id: 0,
      assemblyId: currentAssembly.id,
      priceEach: 0.00,
      inventory: my.inventory,
      quantity: q,
    };
    const a = currentAssembly;
    a.subItems.push(baseItem);
    return API.editAssembly(currentAssembly.id, a)
      .then(() => {
        successAlert("Sub item added.");
        f.reset();
        return listAssemblies()
          .then(() => { openSubItems(currentAssembly.id) })
          .catch(errorAlert);
      })
      .catch(errorAlert);
  };
};

function addSubItemPurchase(event) {
  return addSubItem("purchase", "purchaseQuant", "Purchase", purchasesMap)(event);
};

function addSubItemFab(event) {
  return addSubItem("fabrication", "fabricationQuant", "Fabrication", fabricationsMap)(event);
};

function addSubItemMaterial(event) {
  return addSubItem("material", "materialQuant", "Material", materialsMap)(event);
};

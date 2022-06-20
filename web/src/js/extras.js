"use strict";

var extrasMap = new Map();
var purchasesMap = new Map();
var fabricationsMap = new Map();
var categoriesMap = new Map();
var extrasToAdd = [];
const DATALISTS_EXTRAS = [
  {dlid: "list_fabrication", inputId: "f_fabrication", isEdit: false},
  {dlid: "list_purchase", inputId: "f_purchase", isEdit: false},
  {dlid: "list_category", inputId: "f_category", isEdit: false},
];

function populateFabricationList() {
  return API.listFabItems()
    .then((fs) => {
      fabricationsMap = new Map(fs.map((f) => [f.name, f]));
      setupExtrasList([DATALISTS_EXTRAS[0]], fabricationsMap);
      popListsInner([DATALISTS_EXTRAS[0]], fs);
    })
    .catch(errorAlert);
};

function populatePurchaseList() {
  return API.listPurchaseItems()
    .then((ps) => {
      purchasesMap = new Map(ps.map((p) => [p.name, p]));
      setupExtrasList([DATALISTS_EXTRAS[1]], purchasesMap);
      popListsInner([DATALISTS_EXTRAS[1]], ps);
    })
    .catch(errorAlert);
};

// TODO can probably remove along with all extras stuff
function populateCategoryList() {
  return API.listCategories()
    .then((cs) => {
      const withInvCs = cs.map((c) => {
        // this field is required to match the other 'actual' item data structure
        c.inventory = { type: "category" };
        return c;
      });
      categoriesMap = new Map(withInvCs.map((c) => [c.name, c]));
      setupCategoryDatalists([DATALISTS_EXTRAS[2]], categoriesMap);
      popListsInner([DATALISTS_EXTRAS[2]], withInvCs);
    })
    .catch(errorAlert);
};

function popExtrasTable(tbid, markupFn) {
  return function(exs) {
    const tb = getTableBody(tbid);
    tb.innerHTML = "";
    exs.forEach((ex) => {
      const nextRow = tb.insertRow(-1);
      const markup = markupFn(ex);
      nextRow.innerHTML = markup;
    });
    extrasMap = new Map(exs.map((ex) => [ex.id, ex]));
    return;
  };
};

function mkExtra(pid, fd, baseItem=null) {
  const defObjShape = { projectId: pid };
  const baseFields = [
    {f: "categoryId", parser: safeParseInt},
    {f: "categoryName"},
    {f: "nameCheck"},
  ];
  const editFields = [];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function joinExtras() {
  const prevExtras = [...extrasMap.values()];
  prevExtras.forEach((pe) => {
    const idx = extrasToAdd.findIndex((e) => {
      return pe.categoryId === e.categoryId &&
             pe.categoryName === e.categoryName &&
             pe.nameCheck === e.nameCheck;
    });
    // found thus repeat
    if (idx !== -1) {
      // remove from list
      extrasToAdd.splice(idx, 1);
    }
  });
  return prevExtras.concat(extrasToAdd);
};

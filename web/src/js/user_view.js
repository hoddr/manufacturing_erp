"use strict";

var currentUserPermissions;
const IDKEY = "uid";
const EDIT_FORM = "f_edit_userPermissions";

popSidebar("");

// init
Promise.all([
  getUserPermissions(),
])
  .then(() => {
    successAlert("User permissions loading complete.");
    return;
  })
  .catch(errorAlert);

setFormListenerWrapper(EDIT_FORM, editUserPermissions);

function getUserPermissions() {
  const muid = getQSId(IDKEY, id => id)();
  return API.getUserPermissions(muid)
    .then((up) => {
      currentUserPermissions = up;
      return fillUserPermissions(up);
    })
    .catch(errorAlert);
};

function fillUserPermissions(up) {
  [ {id: "f_assembliesEdit", val: up.assemblies},
    {id: "f_customersEdit", val: up.customers},
    {id: "f_fabricationEdit", val: up.fabrication},
    {id: "f_feedbackEdit", val: up.feedbackComplaints},
    {id: "f_inventoryEdit", val: up.inventory},
    {id: "f_materialsEdit", val: up.materials},
    {id: "f_ordersEdit", val: up.orders},
    {id: "f_pricingEdit", val: up.pricing},
    {id: "f_pricingListsEdit", val: up.pricingLists},
    {id: "f_projectsEdit", val: up.projects},
    {id: "f_purchaseEdit", val: up.purchase},
    {id: "f_quotesEdit", val: up.quotes},
    {id: "f_usersEdit", val: up.users},
    {id: "f_vendorsEdit", val: up.vendors},
    {id: "f_idEdit", val: up.id},
    {id: "f_userIdEdit", val: up.userId},
    {id: "f_purchaseOrdersEdit", val: up.purchaseOrders},
    {id: "f_errorLogEdit", val: up.errorLog},
    {id: "f_curbCoEdit", val: up.curbCo},
  ].forEach(setInputVal);
};

function editUserPermissions(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const muid = getQSId(IDKEY, id => id)();
  return API.editUserPermissions(muid, mkUserPermissions(fd))
    .then(() => {
      successAlert("User permissions edited.");
      return getUserPermissions();
    })
    .catch(errorAlert);
};

function mkUserPermissions(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "assemblies", parser: safeParseInt},
    {f: "customers", parser: safeParseInt},
    {f: "fabrication", parser: safeParseInt},
    {f: "feedbackComplaints", parser: safeParseInt},
    {f: "inventory", parser: safeParseInt},
    {f: "materials", parser: safeParseInt},
    {f: "orders", parser: safeParseInt},
    {f: "pricing", parser: safeParseInt},
    {f: "pricingLists", parser: safeParseInt},
    {f: "projects", parser: safeParseInt},
    {f: "purchase", parser: safeParseInt},
    {f: "quotes", parser: safeParseInt},
    {f: "users", parser: safeParseInt},
    {f: "vendors", parser: safeParseInt},
    {f: "purchaseOrders", parser: safeParseInt},
    {f: "errorLog", parser: safeParseInt},
    {f: "curbCo", parser: safeParseInt},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
    {f: "userId", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

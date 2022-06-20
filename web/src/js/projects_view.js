"use strict";

var projectsMap = new Map();
var customersMap = new Map();
var currentProject;
var showActive = true;
var ordersToManage = [];
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const PIDKEY = "prjid";
const TBID = "t_projects";
const TBID_EXTRAS = "t_project_extras";
const EDIT_FORM = "f_edit_project";
const EXTRA_FORM = "f_add_extra";
const DATALISTS = [
  {dlid: "list_customersEdit", inputId: "f_customerEdit", isEdit: true},
];
const SUBVIEWS = [
  {id: "t_projects", displayStyle: "", viewKey: "main", fn: () => { currentProject = null; }}, // 0
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(PIDKEY, editProjectOpen)}, // 1
  {id: "d_sub_extras", displayStyle: "flex", viewKey: "extras", fn: getQSId(PIDKEY, showExtras)}, // 2
  {id: "t_project_extras", displayStyle: "", viewKey: "extras"}, // 3
];

const mainOpenInner = mainOpenWrapper(SUBVIEWS, [PIDKEY]);
const mainOpen = () => {
  mainOpenInner();
  currentProject = null;
};

popSidebar("Projects");

// init
listProjects()
  .then(() => {
    successAlert("Projects view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return Promise.all([
      populateCustomerList(),
      populateFabricationList(),
      populatePurchaseList(),
    ]).catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(EDIT_FORM, editProject);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listProjects, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

function listProjects() {
  const fn = showActive ? API.listProjectSkeletons : API.listProjectSkeletonsInactive;
  return fn(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function populateCustomerList() {
  return API.listCustomers()
    .then((cs) => {
      customersMap = new Map(cs.map((c) => [c.name, c]));
      setupCustomerDatalists(DATALISTS, customersMap);
      popListsInner(DATALISTS, cs);
    })
    .catch(errorAlert);
};

function popTable(ps) {
  return new Promise((resolve, reject) => {
    const tb = getTableBody(TBID);
    tb.innerHTML = "";
    ps.forEach((p) => {
      const nextRow = tb.insertRow(-1);
      const markup = `
        <td><a href="<<BASE_URL>>/views/project_view.html?prjid=${p.id}">${p.appId}</a></td>
        <td>${p.quoteNumber}</td>
        <td>${p.name}</td>
        <td>${p.po}</td>
        <td>${renderCustomerVendor(p.customer.name)}</td>
        ${renderActiveStatusTD(p.isActive, p.id)}
        <td onclick="editProjectOpen(${p.id});"><i class="fa fa-pencil"></i></td>
        <td onclick="confirmDeleteProject(${p.id})(event);"><i class="fa fa-trash"></i></td>
      `;
      nextRow.innerHTML = markup;
    });
    projectsMap = new Map(ps.map((p) => [p.id, p]));
    return resolve();
  });
};

function editProjectOpen(pid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  setQueryString([[PIDKEY, pid]]);
  return fillEditProject(pid);
};

function fillEditProject(pid) {
  const p = projectsMap.get(pid);
  [ {id: "f_nameEdit", val: p.name},
    {id: "f_appIdEdit", val: p.appId},
    {id: "f_poEdit", val: p.po},
    {id: "f_customerEdit", val: p.customer.name},
    {id: "f_customerIdEdit", val: p.customer.id},
    {id: "f_customerNameEdit", val: p.customer.name},
    {id: "f_customerCompanyEdit", val: p.customer.company},
    {id: "f_customerTypeEdit", val: p.customer.type},
    {id: "f_customerMarkupEdit", val: p.customer.markup},
    {id: "f_totalEstimatedWeightEdit", val: p.totalEstimatedWeight},
    {id: "f_quoteIdEdit", val: p.quoteId},
    {id: "f_quoteNumberEdit", val: p.quoteNumber},
    {id: "f_idEdit", val: p.id},
  ].forEach(setInputVal);
  return;
};

function editProject(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const pid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(pid)) {
    errorAlert("Invalid project id provided. Contact system admin.");
    return;
  }
  return API.editProject(pid, mkProject(fd))
    .then(() => {
      event.target.reset();
      infoAlert("Project edit complete.");
      mainOpen();
      return listProjects();
    })
    .catch(errorAlert);
};

function mkProject(fd, baseItem=null) {
  const defObjShape = { extras: [], orders: [], customer: {}, isActive: true };
  const baseFields = [
    {f: "name"},
    {f: "appId", parser: safeParseInt},
    {f: "po"},
    {f: "customerName", actual: "name", isNested: true, intField: "customer"},
    {f: "customerId", actual: "id", parser: safeParseInt, isNested: true, intField: "customer"},
    {f: "customerCompany", actual: "company", isNested: true, intField: "customer"},
    {f: "customerType", actual: "type", isNested: true, intField: "customer"},
    {f: "customerMarkup", actual: "markup", parser: safeParseDouble, isNested: true, intField: "customer"},
    {f: "totalEstimatedWeight", parser: safeParseDouble},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
    {f: "quoteId", parser: safeParseInt},
    {f: "quoteNumber"},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function showExtras(pid) {
  if (pid === null || pid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[2]);
  display(SUBVIEWS[3]);
  setQueryString([[PIDKEY, pid]]);
  return fillShowExtras(pid);
};

function fillShowExtras(pid) {
  return API.getProject(pid)
    .then((p) => {
      currentProject = p;
      popExtrasTable(TBID_EXTRAS, markupFn)(p.extras);
      return;
    })
    .catch(errorAlert);

  function markupFn(ex) {
    return `
      <td>${ex.id}</td>
      <td>${ex.categoryName}</td>
      ${renderNullTD(ex.nameCheck)}
      <td><button id="${ex.projectId}" class="table-button" onclick="removeExtra(${ex.id}, event);">X</button></td>
    `;
  };
};

function setName(event) {
  event.stopPropagation();
  event.preventDefault();
};

function addExtra(event) {
  event.preventDefault();
  event.stopPropagation();
  const n = document.getElementById(EXTRA_FORM);
  const fd = new FormData(n);
  if (!currentProject || !currentProject.id) {
    errorAlert("Current project not set. Contact system admin.");
    return;
  }
  const baseItem = {
    id: 0, // default placeholder
    projectId: currentProject.id,
  };
  const ex = mkExtra(currentProject.id, fd, baseItem);
  extrasToAdd.push(ex);
  n.reset();
  infoAlert("New extra added.");
  return;
};

function sendExtras(event) {
  event.preventDefault();
  event.stopPropagation();
  if (extrasToAdd.length === 0) {
    errorAlert("No extras to add. Cancelled.");
    return;
  }
  if (!currentProject) {
    errorAlert("Current project not set. Contact system admin.");
    return;
  }
  const toSet = joinExtras();
  return API.setExtras(currentProject.id, toSet)
    .then(() => {
      successAlert("Extras added to project.");
      extrasToAdd = [];
      return listProjects()
        .then(() => {
          showExtras(currentProject.id);
        })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

function removeExtra(exid, event) {
  event.stopPropagation();
  event.preventDefault();
  const pid = event.target.id;
  if (!pid) {
    infoAlert("Project id not set. Contact system admin.");
    return;
  }
  return API.deleteProjectExtra(pid, exid)
    .then(() => {
      successAlert("Extra removed.");
      return listProjects()
        .then(() => {
          showExtras(currentProject.id);
        })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

// TODO consider using helper
function confirmDeleteProject(pid) {
  return function inner(event) {
    event.stopPropagation();
    event.preventDefault();
    closeDialogs();
    return confirmDialog("Are you sure you want to delete this project? All data and extras will be lost.",
      cbConfirmProjectDel(pid),
      cbCancelProjectDel);
  };
};

function cbConfirmProjectDel(pid) {
  return function inner(event) {
    event.stopPropagation();
    event.preventDefault();
    closeDialogs();
    return promptDialog(`Please enter: "Yes" in the prompt to confirm deletion.`,
      cbPromptProjectDel(pid));
  };
};

function cbPromptProjectDel(pid) {
  return function inner(event) {
    event.stopPropagation();
    event.preventDefault();
    const fd = new FormData(event.target);
    event.target.reset();
    const txt = fd.get("prompt");
    if (txt !== "Yes") {
      closeDialogs();
      return infoAlert(`Invalid entry: "${txt}": cancelling action.`);
    }
    return API.deleteProject(pid)
      .then(() => {
        closeDialogs();
        successAlert("Project deletion successful.");
        return listProjects();
      })
      .catch((e) => {
        closeDialogs();
        return errorAlert(e);
      });
  };
};

function cbCancelProjectDel() {
  closeDialogs();
  return infoAlert("Project deletion cancelled.");
};

function getStatusReport(event) {
  event.preventDefault();
  event.stopPropagation();
  return API.getProjectStatusReport()
    .then(() => {
      successAlert("Project status report generation successful.");
      return;
    })
    .catch(errorAlert);
};

function toggleShowHideActive(event) {
  event.preventDefault();
  event.stopPropagation();
  showActive = !showActive;
  return listProjects();
};

function confirmToggleActiveStatus(pid, isActive) {
  closeDialogs();
  return confirmDialog("Please verify that you want the project changed.",
    cbConfirmActiveStatusToggle(pid, isActive),
    () => {closeDialogs(); infoAlert("Active status change canceled.");});
};

function cbConfirmActiveStatusToggle(pid, isActive) {
  return function inner() {
    closeDialogs();
    if (isActive) {
      return API.setProjectToInactive(pid)
        .then(() => {
          successAlert("Project set to inactive.");
          return listProjects();
        })
        .catch(errorAlert);
    }
    return API.setProjectToActive(pid)
      .then(() => {
        successAlert("Project set to active.");
        return listProjects();
      })
      .catch(errorAlert);
  };
};

function manageNonCompleteProjectOrders(event) {
  event.preventDefault();
  event.stopPropagation();
  infoAlert("Retrieving list of project orders not yet complete or billed.");
  return API.listNonCompleteProjectOrders()
    .then((os) => {
      // returned as order number only! not objects
      closeDialogs();
      infoAlert("Checking app for latest order status...");
      return Promise.allSettled(os.map((o) => { return API.checkAndUpdateOrderStatus(o); }))
        .then(() => {
          closeDialogs();
          successAlert("All project order statuses updated. Ready for billing.");
          return listProjects();
        })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

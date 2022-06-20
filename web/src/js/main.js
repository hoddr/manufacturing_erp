// can export all as window.TimeStudyApp or the like if desired to namescpace

'use strict';

const STG_KEY = "erp_app_key";
const SEARCH_KEY = "search";
const LIMIT_KEY = "limit";
const PAGE_KEY = "page";
var searchQS;
var limitQS;
var pageQS;

function updateListQS(cb, event) {
  event.preventDefault();
  event.stopPropagation();
  const { limit, page, search } = updateSearchPagination();
  limitQS = limit;
  pageQS = page;
  searchQS = search;
  return cb();
};

// the following is meant to permit secure (relatively) sharing of creds amongst tabs/windows
// to forgo multiple logins
window.addEventListener("storage", (event) => {
  const creds = retrieveTokenCreds(true);
  if (event.key === "REQUESTING_SHARED_CREDENTIALS" && creds && !event.oldValue) {
    window.localStorage.setItem("CREDENTIALS_SHARING", creds);
    window.localStorage.removeItem("CREDENTIALS_SHARING");
  }
  if (event.key === "CREDENTIALS_SHARING" && (creds === "undefined" || creds === undefined || creds === null || creds === "")) {
    // ensure that we don't set to undefined on the remove event
    if (!event.oldValue) {
      setTokenCreds({"token": event.newValue});
    }
  }
  if (event.key === "CREDENTIALS_FLUSH" && creds && !event.oldValue) {
    removeTokenCreds();
  }
});
const credsTemp = retrieveTokenCreds(true);
if (credsTemp === "undefined" || credsTemp === undefined || credsTemp === null || credsTemp === "") {
  window.localStorage.setItem("REQUESTING_SHARED_CREDENTIALS", "foobar");
  window.localStorage.removeItem("REQUESTING_SHARED_CREDENTIALS", "foobar");
}

function removeTokenCreds() {
  return window
    .sessionStorage
    .removeItem(STG_KEY);
};

function setTokenCreds(token) {
  return window
    .sessionStorage
    .setItem(STG_KEY, token.token);
};

function retrieveTokenCreds(fromEvent=false) {
  const mKey = window
    .sessionStorage
    .getItem(STG_KEY);
  if (!fromEvent && (!mKey || mKey == "")) {
    return goTo(url("views/login.html"));
  }
  return mKey;
};

function popSidebar(currentPage="") {
  const n = document.getElementById("sidebar");
  const entries = [
    {displayName: "Main", href: url("views/main_view.html"), icon: "fa-home"},
    {displayName: "Addresses", href: url("views/address_view.html"), icon: "fa-map-marker"},
    {displayName: "Assemblies", href: url("views/assemblies_view.html"), icon: "fa-sitemap"},
    {displayName: "CC", href: url("views/cc_quotes_view.html"), icon: "fa-graduation-cap"},
    {displayName: "Customers", href: url("views/customers_view.html"), icon: "fa-address-book"},
    {displayName: "Error Log", href: url("views/error_log_view.html"), icon: "fa-exclamation-circle"},
    {displayName: "Fabrication", href: url("views/fab_items_view.html"), icon: "fa-cogs"},
    {displayName: "Feedback Complaints", href: url("views/feedback_complaints_view.html"), icon: "fa-exclamation-triangle"},
    {displayName: "Inventory", href: url("views/inventory_view.html"), icon: "fa-database"},
    {displayName: "Materials", href: url("views/materials_view.html"), icon: "fa-cog"},
    {displayName: "Material Purchases", href: url("views/mat_purchases_view.html"), icon: "fa-gears"},
    {displayName: "Orders", href: url("views/orders_view.html"), icon: "fa-cube"},
    {displayName: "Pricing", href: url("views/pricing_view.html"), icon: "fa-money"},
    {displayName: "Pricing Lists", href: url("views/pricing_lists_view.html"), icon: "fa-tags"},
    {displayName: "Projects", href: url("views/projects_view.html"), icon: "fa-cubes"},
    {displayName: "Purchase Items", href: url("views/purchase_items_view.html"), icon: "fa-bank"},
    {displayName: "Purchase Orders", href: url("views/purchase_orders_view.html"), icon: "fa-shopping-cart"},
    {displayName: "Quotes", href: url("views/quotes_view.html"), icon: "fa-quote-left"},
    {displayName: "Users", href: url("views/users_view.html"), icon: "fa-user-plus"},
    {displayName: "Vendors", href: url("views/vendors_view.html"), icon: "fa-address-card"},
    {displayName: "Logout", href: "javascript:void(0);", icon: "fa-sign-out"},
  ];
  var markup = "";
  entries.forEach((e) => {
    const href = e.displayName === currentPage ?
      "javascript:void(0);" :
      e.href;
    const onlick = e.displayName === "Logout" ?
      "API.logout(event);" :
      "{return true;}";
    markup += `
      <div class="sidebar-entry">
        <a href="${href}" onclick="${onlick}"><i class="fa ${e.icon}"></i> ${e.displayName}</a>
      </div>
    `;
  });
  n.innerHTML = markup;
  return;
};

const setupVendorDatalists = setupDatalistsGeneric(
  [
    {id: "f_vendorId", prop: "id"},
    {id: "f_vendorName", prop: "name"},
    {id: "f_vendorCompany", prop: "company"},
  ], (val) => `Chosen vendor ${val} is not in the supported vendors list. Please add this vendor before adding any associated items.`);

const setupCustomerDatalists = setupDatalistsGeneric(
  [
    {id: "f_customerId", prop: "id"},
    {id: "f_customerName", prop: "name"},
    {id: "f_customerCompany", prop: "company"},
    {id: "f_customerType", prop: "type"},
    {id: "f_customerMarkup", prop: "markup"},
    {id: "f_customerTaxExempt", prop: "isTaxExempt"},
  ], (val) => `Chosen customer ${val} is not in the supported customers list. Please add this customer before adding any associated projects.`);

const setupExtrasList = setupDatalistsGeneric(
  [
    {id: "f_nameCheck", prop: "name"},
  ], (val) => `Chosen item ${val} is not in the supported list. Please add this item for use in extras.`);

const setupUserDatalists = setupDatalistsGeneric(
  [
    {id: "f_userId", prop: "id"},
    {id: "f_userRole", prop: "role"},
    {id: "f_userUsername", prop: "username"},
    {id: "f_userLast", prop: "last"},
    {id: "f_userFirst", prop: "first"},
  ], (val) => `Chosen user ${val} is not in the supported list. Please add this user first.`);

const setupCategoryDatalists = setupDatalistsGeneric(
  [
    {id: "f_categoryId", prop: "id"},
    {id: "f_categoryName", prop: "name"},
  ], (val) => `Chosen category ${val} is not in the supported list.`);

const setupMaterialDatalists = setupDatalistsGeneric(
  [
    {id: "f_materialId", prop: "id"},
  ], (val) => `Chosen material ${val} is not in the supported materials list. Please add this material first.`);

const setupAddressDatalists = setupDatalistsGeneric(
  [
    {id: "f_addressId", prop: "id"},
  ], (val) => `Chosen address ${val} is not in the supported address book. Please add this address first.`);

function setupDatalistsGeneric(fieldsToAdjust, errFn) {
  return function innerSetup(dls, map) {
    dls.forEach(({dlid, inputId, isEdit}) => {
      const n = document.getElementById(inputId);
      n.addEventListener("change", function(event) {
        const maybe = map.get(event.target.value);
        if (maybe === undefined) {
          errorAlert(errFn(event.target.value));
          event.target.value = "";
          return;
        }
        fieldsToAdjust.forEach(({id, prop, innerProp}) => {
          const idFixed = isEdit ? id + "Edit" : id;
          const inp = document.getElementById(idFixed);
          if (innerProp) {
            inp.value = maybe[prop][innerProp];
          } else {
            inp.value = maybe[prop];
          }
        });
      });
    });
  };
};

function popListsInner(dls, xs) {
  var markup = "";
  xs.forEach((x) => {
    markup += `
      <option id="li_${x.id}" value="${x.name}">${x.name}</option>
    `;
  });
  dls.forEach(({dlid}) => {
    const n = document.getElementById(dlid);
    n.innerHTML = "";
    n.innerHTML = markup;
  });
};

function mkObj(defaultObj, baseObj, fd, baseFields, editFields) {
  var fields = baseFields;
  var o;
  if (baseObj === null) {
    fields = baseFields.concat(editFields);
    o = defaultObj;
  } else {
    o = baseObj;
  }
  fields.forEach(({f, isNested, intField, actual, parser=idParse}) => {
    const [errString, val] = parser(fd.get(f));
    if (errString !== null) {
      errorAlert(`Parse error for field: ${f}.\n${errString}`);
      return null;
    }
    const fname = !actual ? f : actual;
    if (isNested) {
      o[intField][fname] = val;
    } else {
      o[fname] = val;
    }
  });
  return o;
};

function setFormListenerWrapper(id, cb) {
  const n = document.getElementById(id);
  n.addEventListener("submit", cb);
};

function setClickListenerCheckboxWrapper(onText, offText) {
  return function({ btnid, inpid }) {
    const btn = document.getElementById(btnid);
    const inp = document.getElementById(inpid);
    btn.addEventListener("click", function(event) {
      event.stopPropagation();
      event.preventDefault();
      if (btn.value === "on") {
        // set to off
        inp.checked = false;
        btn.value = "off";
        btn.innerHTML = offText;
      } else {
        // set to on
        inp.checked = true;
        btn.value = "on";
        btn.innerHTML = onText;
      }
    });
  };
};

function getTableBody(id) {
  return document
    .getElementById(id)
    .getElementsByTagName("tbody")[0];
};

function getCurrPath() {
  return (new URL(document.location))
    .pathname
    .split("/")
    .filter((s) => s !== "");
};

// TODO refactor more or less the same as renderBlankCol
function renderNullTD(val) {
  return `
    <td>${val ? val : "--"}</td>
  `;
};

function renderBoolTD(val) {
  return val ?
    `<td style="color: green;"><i class="fa fa-check"></i></td>` :
    `<td style="color: red;"><i class="fa fa-times"></i></td>`;
};

function renderBilledStatusTD(val, id) {
  const s = `confirmToggleBilledStatus(${id}, ${val})`;
  return val ?
    `<td onclick="${s};" style="color: green;"><i class="fa fa-check"></i></td>` :
    `<td onclick="${s};" style="color: red;"><i class="fa fa-times"></i></td>`;
};

function renderCompleteStatusTD(val, id) {
  const s = `confirmToggleCompleteStatus(${id}, ${val})`;
  return val ?
    `<td onclick="${s};" style="color: green;"><i class="fa fa-check"></i></td>` :
    `<td onclick="${s};" style="color: red;"><i class="fa fa-times"></i></td>`;
};

function renderActiveStatusTD(val, id) {
  const s = `confirmToggleActiveStatus(${id}, ${val})`;
  return val ?
    `<td onclick="${s};" style="color: green;"><i class="fa fa-check"></i></td>` :
    `<td onclick="${s};" style="color: red;"><i class="fa fa-times"></i></td>`;
};

function renderConfirmedStatusTD(val, pid) {
  const s = `confirmToggleConfirmStatus(${pid}, ${val})`;
  return val ?
    `<td onclick="${s};" style="color: green;"><i class="fa fa-check"></i></td>` :
    `<td onclick="${s};" style="color: red;"><i class="fa fa-times"></i></td>`;
};

function confirmToggleConfirmStatusWrapper(confCb) {
  return function(ccqid, isConfirmed) {
    closeDialogs();
    return confirmDialog("Are you sure you want the change the confirmation status for this CC quote?",
    confCb(ccqid, isConfirmed),
    () => { closeDialogs(); infoAlert("Confirmation status change canceled."); });
  };
};

function confirmToggleBilledStatusWrapper(confCb) {
  return function(oid, isBilled) {
    closeDialogs();
    return confirmDialog("Are you sure you want to change the billing status for this order?",
    confCb(oid, isBilled),
    () => {closeDialogs(); infoAlert("Billing status change canceled.");});
  };
};

function confirmToggleCompleteStatusWrapper(confCb) {
  return function(oid, isComplete) {
    closeDialogs();
    return confirmDialog("Are you sure you want to change the completion status for this order?",
      confCb(oid, isComplete),
      () => {closeDialogs(); infoAlert("Completion status change canceled.");});
  };
};

function confirmToggleTaxExemptWrapper(confCb) {
  return function(cid, isTaxExempt) {
    closeDialogs();
    return confirmDialog("Are you sure you want to change the tax exemption status of this customer?",
      confCb(cid, isTaxExempt),
      () => {closeDialogs(); infoAlert("Tax exemption status change canceled.")});
  };
};

function renderDateTD(val) {
  return `<td>${formatDateString(val)}</td>`;
};

function formatDateString(d) {
  if (d === null || d === undefined || d === "") {
    return " -- ";
  }
  const nd = new Date(d);
  const ds = nd.toUTCString();
  return ds.slice(5, 16);
};

function htmlToNewTabWindow(dt) {
  const newWindow = window.open();
  newWindow.document.write(dt);
  return;
};

function promptDownload(blob, name) {
  const dlURL = URL.createObjectURL(blob);
  return promptDL(dlURL, name);
};

function promptDL(dlUrl, name) {
  const a = document.createElement("a");
  a.href = dlUrl;
  a.download = `${name}`;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  return;
};

function promptCSVDownload(d, name) {
  const blob = new Blob(d, {type: "text/csv"});
  const dlUrl = URL.createObjectURL(blob);
  return promptDL(dlUrl, name);
};

function popup(type) {
  return function render(msg) {
    var msgToShow = msg;
    if (msg.hasOwnProperty && msg.hasOwnProperty("ok")) {
      if (!msg.ok) { // error
        return msg.text() // get error msg text
          .then(makePopup(type))
          .catch(errorAlert);
      }
    }
    return makePopup(type, msgToShow);
  };
};

function makePopup(type, msg) {
  const n = document.createElement("div");
  n.className = `alert ${type}`;
  n.innerHTML = msg;
  n.addEventListener("click", fadeOut(2000));
  setTimeout(() => {
    n.style.opacity = "0";
    n.style.display = "none";
  }, 8000);
  document.body.appendChild(n);

  function fadeOut(timeMs) {
    return function(evt) {
      const t = evt.target;
      t.style.opacity = "0";
      setTimeout(() => {
        t.style.display = "none";
      }, timeMs);
    };
  };
};

function hide(views) {
  views.forEach((v) => {
    const n = document.getElementById(v.id);
    n.style.display = "none";
  });
};

function show(view) {
  if (view.viewKey === "main") {
    removeViewQS();
    return;
  }
  setQueryString([["v", view.viewKey]]);
  return;
};

function display(view) {
  const n = document.getElementById(view.id);
  n.style.display = view.displayStyle;
  return;
};

function removeViewQS() {
  const qps = new URLSearchParams(window.location.search);
  qps.delete("v");
  if (qps.toString() === "") {
    history.pushState(null, null, window.location.pathname);
    return;
  }
  history.pushState(null, null, `?${qps.toString()}`);
};

function setQueryString(ps) {
  const qps = new URLSearchParams(window.location.search);
  ps.forEach(([k, v]) => {
    qps.set(k, v);
  });
  history.pushState(null, null, `?${qps.toString()}`);
  return;
};

// TODO not used at the moment
function checkCurrentView(SUBVIEWS) {
  const qps = new URLSearchParams(window.location.search);
  const mcv = qps.get("v");
  if (!mcv) {
    return null;
  }
  mmatch = SUBVIEWS.filter((v) => v.viewKey === mcv);
  if (mmatch.length === 0) {
    return null;
  }
  return mmatch[0];
};

function setInputVal({id, val, isCheckBox=false, onText="", offText="", btnid=null, isSelect=false}) {
  (document.getElementById(id))[isCheckBox ? "checked" : "value"] = val;
  if (isCheckBox) {
    const btn = document.getElementById(btnid);
    btn.value = val ? "on" : "off";
    btn.innerHTML = val ? onText : offText;
  }
  if (isSelect) {
    const s = document.getElementById(id);
    for (let i = 0; i < s.options.length; i++) {
      if (s.options[i].value === val) {
        s.options[i].selected = true;
        s.selectedIndex = i;
      }
    }
  }
  return;
};

function mainOpenWrapper(subviews, idKeys=[]) {
  return function() {
    hide(subviews);
    display(subviews[0]);
    const qps = new URLSearchParams(window.location.search);
    qps.delete("v");
    idKeys.forEach(idkey => qps.delete(idkey));
    if (qps.toString() === "") {
      history.pushState(null, null, window.location.pathname);
    } else {
      history.pushState(null, null, `?${qps.toString()}`);
    }
  };
};

function popOrderTableWrapper(tbid, isSubView=false) {
  return function(os) {
    const tb = getTableBody(tbid);
    tb.innerHTML = "";
    os.forEach((o) => {
      const editRow = isSubView ?
        `<td><a href="<<BASE_URL>>/views/orders_view.html?v=edit&oid=${o.id}><i class="fa fa-pencil"></i></a></td>` :
        `<td><button class="table-button" onclick="editOrderOpen(${o.id});"><i class="fa fa-pencil"></i></button></td>`;
      const numberRow = o.isProgressiveBill ?
        `<td><a href="<<BASE_URL>>/views/progressive_view.html?oid=${o.id}">${o.number}</a></td>` :
        `<td>${o.number}</td>`;
      const nextRow = tb.insertRow(-1);
      const markup = `
        <td>${o.type}</td>
        ${numberRow}
        <td>${o.po}</td>
        <td>${o.customer.name}</td>
        <td>$${formatPrice(o.total)}</td>
        ${renderDateTD(o.createdAt)}
        ${renderCompleteStatusTD(o.isComplete, o.id)}</td>
        ${renderDateTD(o.completeAt)}
        ${renderBilledStatusTD(o.isBilled, o.id)}
        ${renderDateTD(o.billedAt)}
        ${renderNullTD(o.invoiceNumber)}
        <td><button class="table-button" onclick="openLineItems(${o.id});">(&rarr;) View line items</button></td>
        <td><button class="table-button" onclick="openOrderFlags(${o.id});"><i class="fa fa-info-circle"></i></button></td>
        ${editRow}
        <td><button class="table-button" onclick="confirmOrderDeletion(${o.id});"><i class="fa fa-trash"></i></button></td>
      `;
      nextRow.innerHTML = markup;
    });
    return os;
  };
};

function popLineItemTableWrapper(tbid, canToggleExtra=false) {
  return function(lis) {
    const tb = getTableBody(tbid);
    tb.innerHTML = "";
    lis.forEach((li) => {
      const nextRow = tb.insertRow(-1);
      const extraMarkup = canToggleExtra ?
        (li.isExtra ?
          `<td style="color: green;" onclick="toggleLineItemExtra(${li.id}, ${li.isExtra});"><i class="fa fa-check"></i></td>` :
          `<td style="color: red;" onclick="toggleLineItemExtra(${li.id}, ${li.isExtra});"><i class="fa fa-times"></i></td>`) :
        renderBoolTD(li.isExtra);
      const markup = `
        <td>${li.id}</td>
        <td>${li.description}</td>
        <td>${li.category}</td>
        <td>${li.quantity}</td>
        <td>$${formatPrice(li.price)}</td>
        <td>${li.weight}</td>
        ${extraMarkup}
      `;
      nextRow.innerHTML = markup;
    });
    return;
  };
};

function confirmDialog(txt, cbConfirm, cbCancel) {
  const markup = `
    <h4>${txt}</h4>
    <form id="f_confirm" autocomplete="off">
      <input id="btn_confirm" type="button" value="Confirm" class="dialog" />
      <input id="btn_cancel" type="button" value="Cancel" class="dialog" />
    </form>
  `;
  const d = document.createElement("div");
  d.className = "alert info";
  d.id = "confirm";
  d.innerHTML = markup;
  document.body.appendChild(d);
  const confirmBtn = document.getElementById("btn_confirm");
  const cancelBtn = document.getElementById("btn_cancel");
  confirmBtn.onclick = cbConfirm;
  cancelBtn.onclick = cbCancel;
  return;
};

function promptDialog(txt, cb) {
  const markup = `
    <h4>${txt}</h4>
    <form id="f_prompt_box" autocomplete="off">
      <label for="f_prompt">Enter required prompt and/or hit enter:</label>
      <input type="text" id="f_prompt" name="prompt" />
      <input class="dialog" type="submit" value="Confirm" style="display: none;" />
    </form>
  `;
  const d = document.createElement("div");
  d.className = "alert info";
  d.id = "prompt";
  d.innerHTML = markup;
  d.style.display = "block";
  document.body.appendChild(d);
  const f = document.getElementById("f_prompt_box");
  f.addEventListener("submit", cb);
  document
    .getElementById("f_prompt")
    .focus();
  return;
};

function closeDialogs() {
  const ds = document.getElementsByClassName("alert");
  [...ds].forEach((d) => {
    document.body.removeChild(d);
  });
  return;
};

function formatMaybePrice(mnum) {
  if (mnum === null || mnum === undefined) {
    return 0.00;
  }
  return formatPrice(mnum);
};

function formatPrice(num) {
  if (typeof num === "number") {
    return num.toFixed(2);
  }
  errorAlert("Internal: calling formatPrice on non-numeric value.");
  return num;
};

function formatWeight(num) {
  if (typeof num === "number") {
    return num.toFixed(1);
  }
  errorAlert("Internal: calling formatWeight on non-numeric value.");
  return num;
};

function renderBlankCol(v) {
  return !v ? " - " : v;
};

function renderCustomerVendor(s) {
  return s === "_default" ? " - " : s;
};

const errorAlert = popup("error");
const infoAlert = popup("info");
const successAlert = popup("success");

const goTo = (u) => window.location.assign(u);
const BASE_URL = "<<BASE_URL>>";
const url = (path) => `${BASE_URL}/${path}`;

// all parsers return [errString, val] form
function safeParseMaybeInt(mv) {
  if (mv === null || mv === undefined || mv === "") {
    return [null, null];
  }
  return safeParseInt(mv);
};

function safeParseInt(v) {
  var errString = null;
  const mi = Number.parseInt(v, 10);
  if (Number.isNaN(mi)) {

    errString = "Expected integer in form field: NaN parse result. Contact system admin.";
  }
  return [errString, mi];
};

function safeParseDouble(v) {
  var errString = null;
  const md = Number.parseFloat(v);
  if (Number.isNaN(md)) {
    errString = "Expected number in form field: NaN parse result. Contact system admin.";
  }
  return [errString, md];
};

function safeParseMaybeDouble(mv) {
  if (mv === undefined || mv === null || mv === "") {
    return [null, mv === "" ? null : mv];
  }
  return safeParseDouble(mv);
};

const idParse = (v) => [null, v];

function safeParseBool(v) {
  var errString = null;
  // assume null value is simply false (weird behavior on turning checkbox 'off' fix
  if (v === null) {
    return [errString, false];
  }
  const cv = v.toUpperCase();
  const istrue = cv === "ON" ||
    cv === "TRUE" ||
    cv === "T" ||
    cv === "YES";
  const isfalse = cv === "OFF" ||
    cv === "FALSE" ||
    cv === "F" ||
    cv === "NO";
  if (!istrue && !isfalse) {
    errString = "Invalid bool string options passed to parser. Contact system admin.";
  }
  return [errString, istrue];
};

function safeParseDate(d) {
  const md = new Date(d);
  if (md.toString() === "Invalid Date") {
    return ["Invalid date string provided to parser. Contact system admin.", null];
  }
  return [null, md];
};

function pseudoRoute(views, mainOpen=null) {
  const searchParams = new URLSearchParams((new URL(document.URL)).search);
  const viewValues = searchParams.getAll("v"); // view qs key
  // close all current views
  hide(views);
  if (viewValues.length === 0) {
    display(views[0]);
    return;
  }
  viewValues.forEach((val) => {
    const entry = views.filter((v) => v.viewKey === val);
    display(entry[0]);
    if (entry[0].fn) {
      entry[0].fn();
    }
  });
  return;
};

function getQSId(key, cbfn) {
  return function inner() {
    const searchParams = new URLSearchParams((new URL(document.URL)).search);
    const mval = searchParams.get(key);
    // for debugging only
    // console.log(`key: ${key} with value: ${mval} and search params ${searchParams.toString()}`);
    if (mval !== null) {
      return cbfn(Number.parseInt(mval));
    }
    // for debugging only
    // errorAlert(`Expected value for querystring key: ${key}, but none found.`);
    return cbfn(null);
  };
};

function getPaginationQS(def="100") {
  if (window.performance.navigation.type === window.performance.navigation.TYPE_RELOAD) {
    const msearch = window.sessionStorage.getItem(SEARCH_KEY);
    const search = msearch === null ? "" : msearch;
    const mlimit = window.sessionStorage.getItem(LIMIT_KEY);
    const mpage = window.sessionStorage.getItem(PAGE_KEY);
    if (mlimit === null || mpage === null) {
      return [search, def, "1"];
    }
    return [search, mlimit, mpage];
  }
  return ["", def, "1"];
};

function setPaginationQS(searchQS, limitQS, pageQS) {
  const n = document.getElementById("qs_limit");
  const n2 = document.getElementById("qs_offset");
  const n3 = document.getElementById("qs_search");
  if (!n || !n2 || !n3) {
    return;
  }
  n.value = limitQS;
  n2.value = pageQS;
  n3.value = searchQS;
};

function updateSearchPagination() {
  const n = document.getElementById("qs_limit");
  const n2 = document.getElementById("qs_offset");
  const n3 = document.getElementById("qs_search");
  if (!n || !n2 || !n3) {
    return;
  }
  window.sessionStorage.setItem(LIMIT_KEY, n.value);
  window.sessionStorage.setItem(PAGE_KEY, n2.value);
  window.sessionStorage.setItem(SEARCH_KEY, n3.value);
  return { limit: n.value, page: n2.value, search: n3.value };
};

// modals
function setupModal(mid, sid) {
  const modal = document.getElementById(mid);
  const span = document.getElementById(sid);
  const hideModal = hideModalWrapper(modal);
  const showModal = showModalWrapper(modal);
  span.onclick = hideModal;
  return [modal, hideModal, showModal];
};

function hideModalWrapper(m) {
  return function hider() {
    m.style.display = "none";
  };
};

function showModalWrapper(m) {
  return function shower() {
    m.style.display = "block";
  };
};

function simpleDeleteConfirmationProcess(confirmString, endCb, delString="Deletion canceled.") {
  return function caller(id = -1) {
    return confirmDialog(confirmString, yes1(id), no1(delString));
  };

  function yes1(id) {
    return function inner(event) {
      event.preventDefault();
      event.stopPropagation();
      closeDialogs();
      return promptDialog(`Please enter: "Yes" in the prompt to confirm.`,
        yes2(id));
    };
  };

  function yes2(id) {
    return function inner(event) {
      event.preventDefault();
      event.stopPropagation();
      const fd = new FormData(event.target);
      event.target.reset();
      const txt = fd.get("prompt");
      // failure path step 2
      if (txt !== "Yes") {
        closeDialogs();
        return infoAlert(`Invalid entry: "${txt}": canceling action.`);
      }
      // success 3
      closeDialogs();
      return endCb(id);
    };
  };

  function no1(delString) {
    return function inner() {
      closeDialogs();
      return infoAlert(delString);
    };
  };
};

function setTableCaption(tid, s) {
  const n = document.getElementById(tid);
  n.innerHTML = s;
  return;
};

function setAddressWrapper(fid, apicall) {
  return function(addressMap) {
    return function(event) {
      event.preventDefault();
      event.stopPropagation();
      const f = document.getElementById(fid);
      const fd = new FormData(f);
      const address = fd.get("address");
      const addFull = addressMap.get(address);
      if (addFull === undefined) {
        errorAlert("Specified address not found in map.");
        return;
      }
      const [errString, idval] = safeParseInt(fd.get("assignId"));
      if (errString !== null) {
        errorAlert(`Parse error for field: assignId.\n${errString}.`);
        return;
      }
      const [errString2, addIdVal] = safeParseInt(fd.get("addressId"));
      if (errString2 !== null) {
        errorAlert(`Parse error for field: addressId.\n${errString2}.`);
        return;
      }
      return apicall(addIdVal, idval)
        .then(() => {
          successAlert("Address set.");
          return;
        })
        .catch(errorAlert);
    };
  };
};

function genAddressStrings(as) {
  return as.map((a) => {
    a.name = makeAddressString(a);
    return a;
  });
};

function makeAddressString(a) {
  if (a.id === 0 || !a.street) {
    return "";
  }
  const s2 = a.street2 ? ` ${a.street2}` : "";
  return `${a.street}${s2}, ${a.city}, ${a.state} ${a.zip}`;
};

function listAddresses(dls, addmap) {
  return API.listAddresses()
    .then((as) => {
      const fullAdds = genAddressStrings(as);
      addmap = new Map(fullAdds.map((a) => [a.name, a]))
      popListsInner(dls, fullAdds);
      setupAddressDatalists(dls, addmap);
      return addmap;
    })
    .catch(errorAlert);
};

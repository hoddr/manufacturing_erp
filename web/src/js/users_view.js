"use strict";

var usersMap = new Map();
const IDKEY = "uid";
const TBID = "t_users";
const EDIT_FORM = "f_edit_user";
const ADD_FORM = "f_add_user";
const SUBVIEWS = [
  {id: "t_users", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editUserOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);

popSidebar("Users");

// init
Promise.all([
  listUsers(),
])
  .then(() => {
    successAlert("Users view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addUser);
setFormListenerWrapper(EDIT_FORM, editUser);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listUsers() {
  return API.listUsers()
    .then(popTable)
    .catch(errorAlert);
};

function popTable(us) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  us.forEach((u) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td><a href="<<BASE_URL>>/views/user_view.html?uid=${u.id}">${u.id}</a></td>
      <td>${u.first}</td>
      <td>${u.last}</td>
      <td>${u.username}</td>
      <td>${u.role}</td>
      <td onclick="editUserOpen(${u.id});"><i class="fa fa-pencil"></i></td>
    `;
    nextRow.innerHTML = markup;
  });
  usersMap = new Map(us.map((u) => [u.id, u]));
  return;
};

function addUserOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addUser(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
  };
  return API.addUser(mkUser(fd, baseItem))
    .then(() => {
      event.target.reset();
      infoAlert("New user added.");
      mainOpen();
      return listUsers();
    })
    .catch(errorAlert);
};

function editUserOpen(uid) {
  if (uid === null || uid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, uid]]);
  return fillEditUser(uid);
};

function fillEditUser(uid) {
  const u = usersMap.get(uid);
  [ {id: "f_useridEdit", val: u.id},
    {id: "f_roleEdit", val: u.role},
    {id: "f_usernameEdit", val: u.username},
    {id: "f_lastEdit", val: u.last},
    {id: "f_firstEdit", val: u.first},
  ].forEach(setInputVal);
};

function editUser(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const id = Number.parseInt(fd.get("id"));
  if (Number.isNaN(id)) {
    errorAlert("Invalid user id provided. Contact system admin.");
    return;
  }
  return API.editUser(id, mkUser(fd))
    .then(() => {
      event.target.reset();
      infoAlert("User edit complete.");
      mainOpen();
      return listUsers();
    })
    .catch(errorAlert);
};

function mkUser(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "first"},
    {f: "last"},
    {f: "role"},
    {f: "username"},
    {f: "password"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

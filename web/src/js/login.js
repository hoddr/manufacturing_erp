"use strict";

const FBID = "f_login";
setFormListenerWrapper(FBID, login);
setFocus();

function setFocus() {
  return document
    .getElementById("f_user")
    .focus();
};

function login(event) {
  event.preventDefault();
  event.stopPropagation();
  const formData = new FormData(event.target);
  const [user, pass] = [formData.get("user"), formData.get("pass")];
  // has to use then(onSuccess, onError) pattern for proper handling
  return API.login(user, pass)
    .then(() => {
      return goTo(url("views/main_view.html"));
    }, errorAlert)
    .catch(errorAlert);
};

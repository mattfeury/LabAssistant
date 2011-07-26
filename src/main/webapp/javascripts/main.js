/**
 * Since IE can't handle the heat of HTML5
 */
function $ie(contents) {
  return (typeof(innerShiv) == "undefined") ? $(contents) : $(innerShiv(contents, false));
}

/*
 * Insertion & Removal functions
 * these are the base ones all other ones call
 */
function insertModel(model, id, xhtml, refreshNotReplace) {
  var $model = $ie(xhtml);
  var $existingModel = $('.' + model + '[data-id=' + id + ']');

  if ($existingModel.length && ! refreshNotReplace) //replace
    $existingModel.replaceWith($model);
  else if (refreshNotReplace) //refresh
    location.reload(true);
  else if ($('.' + model + 's').length) //insert
    $('.' + model + 's').append($model);
}

function removeModel(model, id) {
  $('.' + model + '[data-id=' + id + ']').fadeOut(function() {
    $(this).remove();
  });

}

//courses
function insertCourse(id, xhtml) {
  var refresh = false;
  if (isCoursePage === id) refresh = true;

  insertModel('course', id, xhtml, refresh);
}
function removeCourse(id) {
  if (isCoursePage === id) {
    history.back();
    return false;
  }
  removeModel('course', id);
}

//labs
function insertLab(id, xhtml) {
  var refresh = false;
  if (isLabPage === id) refresh = true;

  insertModel('lab', id, xhtml, refresh);
}
function removeLab(id) {
  if (isLabPage === id) {
    history.back();
    return false;
  }
  removeModel('lab', id);
}

//teams
function insertTeam(id, xhtml) {
  insertModel('team', id, xhtml);
}
function removeTeam(id) {
  removeModel('team', id);
}


function showMessage(msg) {
  $('nav .messages .info').slideUp(function() { $(this).remove() });
  $('nav .messages').append(
    $('<div class="info" />')  
      .text(msg)
      .hide()
      .slideDown()
  );
}
function showError(err) {
  $('nav .messages .error').slideUp(function() { $(this).remove() });
  $('nav .messages').append(
    $('<div class="error" />')  
      .text(err)
      .hide()
      .slideDown()
  );

}

var pathParts = location.pathname.split("/")
var isCoursePage = (pathParts.length == 3) ? ((pathParts[1] === 'courses') ? pathParts[2] : null) : null;
var isLabPage = (pathParts.length == 3) ? ((pathParts[1] === 'labs') ? pathParts[2] : null) : null;

$(function() {
  

});

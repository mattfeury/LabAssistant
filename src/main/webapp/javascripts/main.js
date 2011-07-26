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
function insertModel(model, id, xhtml) {
  var $model = $ie(xhtml);
  var $existingModel = $('.' + model + '[data-id=' + id + ']');

  if ($existingModel.length) {
    $existingModel.replaceWith($model);
  } else if ($('.' + model + 's:has(.' + model + ')').length) {
    $('.' + model + 's:has(.' + model + ')').append($model);
  }
}

function removeModel(model, id) {
  $('.' + model + '[data-id=' + id + ']').fadeOut(function() {
    $(this).remove();
  });

}

//courses
function insertCourse(id, xhtml) {
  insertModel('course', id, xhtml);
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
  insertModel('lab', id, xhtml);
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

var pathParts = location.pathname.split("/")
var isCoursePage = (pathParts.length == 3) ? ((pathParts[1] === 'courses') ? pathParts[2] : null) : null;
var isLabPage = (pathParts.length == 3) ? ((pathParts[1] === 'labs') ? pathParts[2] : null) : null;

$(function() {
  

});

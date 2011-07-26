function $ie(contents) {
  return (typeof(innerShiv) == "undefined") ? $(contents) : $(innerShiv(contents, false));
}


function insertCourse(id, xhtml) {
  var $course = $ie(xhtml);
  var $existingCourse = $('.course[data-id=' + id + ']');

  if ($existingCourse.length) {
    $existingCourse.replaceWith($course);
  } else if ($('.courses:has(.course)').length) {
    $('.courses:has(.course)').append($course);
  }

}
function removeCourse(id) {
  $('.course[data-id=' + id + ']').fadeOut(function() {
    $(this).remove();
  });
}

$(function() {
  

});

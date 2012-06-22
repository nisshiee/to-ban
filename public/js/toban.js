$(document).ready(function() {

  $("#assign input:submit").hide();

  $("#assign input:radio").click(function() {
    $("#assign").submit();
  });
});

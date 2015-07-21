$(document).ready(function () {
			   
	$('tbody tr').hover(function() {
	  $(this).addClass('odd');
	}, function() {
	  $(this).removeClass('odd');
	});

});
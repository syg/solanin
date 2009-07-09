$(document).ready(function() {
	progressBar();

	$.ajax({
		type: "POST",
		url: "/_b",
		success: function(data, ts) {
			if(data == "") {
				window.location.reload();
			} else {
				$("#config").html(data);
			}
		}
	});
});

var progress = 1;
function progressBar() {
	if($("#progress").length == 0) {
		return false;
	}

	switch(progress) {
	case 1:
		$("#progress").html('·<span class="hidden">··</span>');
		break;
	case 2:
		$("#progress").html('<span class="hidden">·</span>·<span class="hidden">·</span>');
		break;
	case 3:
		$("#progress").html('<span class="hidden">··</span>·');
		break;
	}

	if(progress >= 3) {
		progress = 1;
	} else {
		progress++;
	}

	setTimeout(progressBar, 1000);
}

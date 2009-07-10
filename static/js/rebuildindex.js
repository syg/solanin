$(document).ready(function() {
	progressBar();

	$.ajax({
		type: "POST",
		url: "/_b",
		success: function(data, ts) {
			if($("#config").length > 0) {
				$("#config").html(data);
			} else {
				var fc = $("#footer-content");
				var pl = $("#playlist");

				pl.html(data);
				solanin.doDirs(pl);
				solanin.doSongs(pl);
				solanin.overflowEllipsis(pl);

				$(".tabs > a").removeClass("selected");
				fc.empty();
				fc.hide();
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

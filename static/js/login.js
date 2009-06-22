$(document).ready(function() {
	$("#login-form").submit(function() {
		$.ajax({
			type: "POST",
			url: $(this).attr("action"),
			data: $(this).serialize(),
			success: function() {
				$("#login-form").unbind();
				$("#login-form").submit(function() { return true; });
				$("#submit").click();
			},
			error: function() {
				p = $("#password");
				p.attr("value", "");
				p.css({ backgroundColor: "#df6161" });
				p.focus();
				p.animate({ backgroundColor: "#d9d9cf" }, 500);
			}
		});

		return false;
	});

	$("#login-password").focus();
});

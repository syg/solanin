$(document).ready(function() {
	new Solanin();
});

function Solanin() {
	var sm = soundManager;
	var sl = this;

	this.songsInMemory = 10;
	this.scrollStopped = false;
	this.scrollDir = "left";
	this.scrollPause = 4000;
	this.scrollSpeed = 40; /* pixels per second */
	this.sounds = [];
	this.currentSound = null;
	this.failedLoad = false;
	this.muted = false;
	this.volume = 100;
	this.volumeStep = 100/15;
	this.lastVolumePopup = 0;

	sm.url = "/_s/swf/";
	sm.debugMode = false;
	sm.useHighPerformance = true;
	sm.onload = function() {
		pl = $("#playlist");
		/* add callbacks for all dirs and songs */
		sl.doDirs(pl);
		sl.doSongs(pl);
		sl.overflowEllipsis(pl);
		sl.doKeybinds();
		sl.doOptions();
		sl.scrollPlaying();

		/* seeking and dragging */
		$("#position, #loading").mousedown(sl.setPosition);
		$("#position, #loading").mouseup(function() {
			sl.currentSound.resume();
		});
		$("#position").bind("drag", sl.setPosition);
		$("#position").bind("dragend", function() {
			sl.currentSound.resume();
		});
	}

	this.events = {
		load: function() {
			var cs = sl.currentSound;
			if(cs.loaded == false) sl.loadingError(cs._mdata.tr);
		},

		play: function() {
			var cs = sl.currentSound;
			var md = cs._mdata;

			if(cs.readyState != 2) {
				sl.showBars();
				var np = md.artist ? md.artist + " - " + md.title : md.title;
				document.title = "Playing: " + np;
				$("#now-playing > span").text(np).removeClass("error");
				md.tr.attr("class", "playing");
				sl.scrollPlaying();
			} else {
				sl.loadingError();
			}
		},

		pause: function() {
			var md = sl.currentSound._mdata;
			document.title = "Paused: " + $("#now-playing > span").text();
			$("#timing-position").css("text-decoration", "blink");
			md.tr.attr("class", "paused");
		},

		resume: function() {
			var md = sl.currentSound._mdata;
			document.title = "Playing: " + $("#now-playing > span").text();
			$("#timing-position").css("text-decoration", "none");
			md.tr.attr("class", "playing");
		},

		stop: function() {
			var md = sl.currentSound._mdata;
			$("#timing-position").text("-:--");
			$("#timing-total").text("-:--");
			sl.hideBars();
			md.tr.removeAttr("class");
		},

		finish: function() {
			/* we have to call stop just in case this is the last song
			 * even though it'll be called again in playNext if this
			   isn't the last song
			 */
			sl.events.stop();
			if(!sl.playNext()) {
				sl.showLogo();
			}
		},

		whileloading: function() {
			/* this doesn't fire in Safari, don't know why */
			var w = this.duration / this._mdata.duration * $("#statusbar").width() - 3;
			$("#loading").width(w < 0 ? "0" : w + "px");
		},

		whileplaying: function() {
			var p = this.position;
			var d = this._mdata.duration;
			var w = p / d * $("#statusbar").width() - 3

			$("#timing-position").text(sl.msToDuration(p));
			$("#timing-total").text(sl.msToDuration(d));
			$("#position").width(w < 0 ? "0" : w + "px");
		}
	}

	this.playPrev = function() {
		if(!sl.currentSound) return false;

		var md = sl.currentSound._mdata;
		if(md.tr.prev("tr").length != 0) {
			sl.events.stop();
			md.tr.prev().find("a").click();
			return true;
		} else {
			return false;
		}
	}

	this.playNext = function() {
		if(!sl.currentSound) return false;

		var md = sl.currentSound._mdata;
		if(md.tr.next("tr").length != 0) {
			sl.events.stop();
			md.tr.next().find("a").click();
			return true;
		} else {
			return false;
		}
	}

	this.popupVolume = function() {
		var d = new Date();
		var volDiv = $("#volume");
		var vol = sl.volume;
		var img = "";

		if(sl.muted) vol = 0;

		if(vol == 0 || sl.muted) {
			img = "mute.png";
		} else if(vol > 0 && vol <= 33) {
			img = "soft.png";
		} else if(vol > 33 && vol <= 66) {
			img = "medium.png";
		} else {
			img = "loud.png";
		}

		volDiv.find("img").attr("src", "/_s/img/" + img).attr("alt", vol + "%");
		$("#volumebar-position").width(vol * 0.99 + "%");
		volDiv.stop();
		volDiv.css("opacity", 1);
		volDiv.show();
		sl.lastVolumePopup = d.getTime();

		setTimeout(function() {
			d = new Date();
			/* give a jitter of 100ms */
			if(d.getTime() >= sl.lastVolumePopup + 1400) {
				volDiv.fadeOut("slow");
			}
		}, 1500);
	}

	this.toggleMute = function() {
		sl.muted = !(sl.muted);
		sl.syncVolume();
	}

	this.volumeDown = function() {
		var dvol = sl.volume - sl.volumeStep;
		sl.volume = dvol < 0 ? 0 : dvol;
		sl.syncVolume();
	}

	this.volumeUp = function() {
		var dvol = sl.volume + sl.volumeStep;
		sl.volume = dvol > 100 ? 100 : dvol;
		sl.syncVolume();
	}

	this.syncVolume = function() {
		var cs = sl.currentSound;
		if(cs) {
			sl.log("Solanin.syncVolume: " + sl.volume + ", muted=" + sl.muted);
			cs.setVolume(sl.volume);
			if(sl.muted) {
				cs.mute();
			} else {
				cs.unmute();
			}
		}
	}

	this.showLogo = function() {
		document.title = "Solanin";
		var np = $("#now-playing > span");
		sl.stopScroll(np);
		np.html('<img alt="welcome to solanin" src="/_s/img/logo.png" />');
	}

	this.sweep = function() {
		if(sl.sounds.length > sl.songsInMemory) {
			sm.destroySound(sl.sounds.shift());
		}
	}

	this.setPosition = function(e) {
		var x = e.clientX;
		var pos = $("#position");
		var sb = $("#statusbar");
		var d = sl.currentSound._mdata.duration;
		var offset = Math.floor((x - pos.offset().left) / sb.width() * d);
		sl.currentSound.setPosition(offset);
	}

	this.showBars = function() {
		$("#timing").show();
		$("#loading").show();
		$("#position").show();
	}

	this.hideBars = function() {
		$("#timing").hide();
		$("#loading").hide();
		$("#position").hide();
		$("#loading").width("0");
		$("#position").width("0");
	}

	this.canPlayURL = function(url) {
		fp = url.split("/").pop();
		if(fp.indexOf("?tr") == fp.length - 3 ||
		   fp.indexOf(".mp3") == fp.length - 4) {
			return true;
		} else {
			return false;
		}
	}

	this.msToDuration = function(ms) {
		var secs0 = Math.floor(ms / 1000);
		var mins = Math.floor(secs0 / 60);
		var secs = secs0 - (mins * 60);
		return (mins + ":" + (secs < 10 ? "0" + secs : secs));
	}

	this.durationToMs = function(str) {
		var ss = str.split(":");
		var seconds = 0;
		for(var i = ss.length; i--;) {
			seconds += parseInt(ss[i], 10) * Math.pow(60, ss.length - i - 1, 10);
		}
		return (seconds * 1000);
	}

	this.loadingError = function(tr) {
		e = "Could not load";
		$("#now-playing > span").text(e).addClass("error");
		tr.removeAttr("class");
		sl.hideBars();
		sm.destroySound(sl.currentSound.sID);
		sl.sounds.pop();
	}

	this.doKeybinds = function() {
		var fc = $("#footer-content");
		$(document).keypress(function(e) {
			/* don't want to trap keyboard shortcuts when editing settings */
			if(fc.html()) return;

			switch(e.which) {
			case 107: /* k */
				sl.playPrev();
				break;
			case 106: /* j */
				sl.playNext();
				break;
			case 63: /* ? */
				$("#keyhelp").toggle();
				break;
			case 117: /* u */
				sm.stopAll();
				sl.showLogo();
				break;
			case 109: /* m */
				sl.toggleMute();
				sl.popupVolume();
				break;
			case 91: /* [ */
				sl.volumeDown();
				sl.popupVolume();
				break;
			case 93: /* ] */
				sl.volumeUp();
				sl.popupVolume();
				break;
			case 112: /* p */
				if(sl.currentSound) sl.currentSound.togglePause();
				break;
			}
		});
	}

	this.startScroll = function(e, d) {
		var f = null;
		sl.scrollStopped = false;

		if(sl.scrollDir == "left") {
			f = function() { sl.scrollLeft(e, d); };
		} else if(sl.scrollDir == "right") {
			f = function() { sl.scrollRight(e, d); };
		}

		setTimeout(f, sl.scrollPause);
	}

	this.stopScroll = function(e) {
		sl.scrollStopped = true;
		e.stop();
		e.unbind(); // unbind the hover callback
		e.css("margin-left", "0px");
	}

	this.scrollLeft = function(e, d) {
		if(sl.scrollStopped) return false;
		sl.scrollDir = "left";

		e.animate({ marginLeft: "-" + d + "px" },
		          d / sl.scrollSpeed * 1000, "linear",
		          function() {
		            setTimeout(function() {
		              sl.scrollRight(e, d);
		            }, sl.scrollPause);
		          });
	}

	this.scrollRight = function(e, d) {
		if(sl.scrollStopped) return false;
		sl.scrollDir = "right";

		e.animate({ marginLeft: "0px" },
		          d / sl.scrollSpeed * 1000, "linear",
		          function() {
		            setTimeout(function() {
		              sl.scrollLeft(e, d);
		            }, sl.scrollPause);
		          });
	}

	this.scrollPlaying = function() {
		var np = $("#now-playing > span");
		sl.stopScroll(np);

		if(np.width() > np.parent().width()) {
			d = np.width() - np.parent().width();
			sl.startScroll(np, d);

			np.hover(function() {
				$(this).stop();
			}, function() {
				sl.startScroll(np, d);
			});
		}
	}

	this.urlToId = function(url) {
		return ("_solanin:" + hex_md5(url));
	}

	this.overflowEllipsis = function(d) {
		d.find(".artist > span, .title > a, .dir > a").each(function() {
			var pw = $(this).parent().width();
			var w = $(this).width();
			var txt = $(this).text();

			if(w <= pw) return;

			$(this).attr("title", txt);

			for(var i = txt.length; i > 0; i--) {
				$(this).text(txt.slice(0, i) + "...");
				w = $(this).width();
				if(w <= pw) return;
			}
		});
	}

	this.doConfigForm = function(a) {
		var fc = $("#footer-content");
		fc.find("#config-form").submit(function() {
			$.ajax({
				type: "POST",
				url: $(this).attr("action"),
				data: $(this).serialize(),
				success: function(data, ts) {
					if(data == "") {
						window.location.reload();
					} else {
						fc.html(data);
						sl.doConfigForm(a);
						sl.doFormCancel();
					}
				}
			});

			return false;
		});
	}

	this.doPasswordForm = function(a) {
		var fc = $("#footer-content");
		var fs = $("#footer-status");
		fc.find("#password-form").submit(function() {
			$.ajax({
				type: "POST",
				url: $(this).attr("action"),
				data: $(this).serialize(),
				success: function(data, ts) {
					if(data == "") {
						$(".tabs > a").removeClass("selected");
						fc.empty();
						fc.hide();
						fs.html("password updated!");
						fs.addClass("update-success");
						setTimeout(function() {
							fs.fadeOut("slow", function() {
								fs.removeClass("update-success");
								fs.html('press <span class="cmd">?</span> for keyboard shortcuts');
								fs.fadeIn("slow");
							});
						}, 3000);
					} else {
						fc.html(data);
						sl.doPasswordForm(a);
						sl.doFormCancel();
					}
				}
			});

			return false;
		});
	}

	this.doFormCancel = function() {
		var fc = $("#footer-content");
		fc.find("input.cancel").click(function() {
			$(".tabs > a").removeClass("selected");
			fc.empty();
			fc.hide();
			return false;
		});
	}

	this.doOptions = function() {
		var fc = $("#footer-content");
		$(".tabs > a[href!=/_x]").click(function() {
			var a = $(this);
			if(!fc.html() || !a.hasClass("selected")) {
				$.get(a.attr("href"), function(data) {
					$(".tabs > a").removeClass("selected");
					a.addClass("selected");
					fc.html(data);
					sl.doConfigForm(a);
					sl.doPasswordForm(a);
					sl.doFormCancel();
					fc.show();
				});
			}

			return false;
		});
	}

	this.doDirs = function(d) {
		d.find(".dir > a").click(function() {
			var a = $(this);
			$.get(a.attr("href"), function(data) {
				a.parent("li").addClass("open-dir");
				a.after(data);
				a.unbind("click");
				a.click(function() {
					$(this).next().toggle();
					$(this).parent("li").toggleClass("open-dir");
					return false;
				});
				sl.doDirs(a.parent());
				sl.doSongs(a.parent());
				sl.overflowEllipsis(a.parent());
			});
			return false;
		});
	}

	this.doSongs = function(d) {
		d.find(".songs a").click(function() {
			var url = $(this).attr("href");
			var id = sl.urlToId(url);

			if(url.indexOf(".mp3") != url.length - 4) {
				url = url + "?tr";
			}

			if(!sl.canPlayURL(url)) {
				return true;
			}

			var ss = sm.getSoundById(id);
			var cs = sl.currentSound;

			if(ss) {
				if(cs) sm.stop(cs.sID);
				sl.currentSound = ss;
				ss.setPosition(0);
				/* already loaded */
				$("#loading").width("100%");
				ss.play();
			} else {
				ss = sm.createSound({
					/* single quotes in the id break SoundManager */
					id: id,
					url: url,
					onload: sl.events.load,
					onplay: sl.events.play,
					onpause: sl.events.pause,
					onresume: sl.events.resume,
					onstop: sl.events.stop,
					onfinish: sl.events.finish,
					whileloading: sl.events.whileloading,
					whileplaying: sl.events.whileplaying
				});

				var atag = $(this).parent().nextAll(".artist").find("span");
				ss._mdata = {
					artist: atag.attr("title") != "" ? atag.attr("title") : atag.text(),
					title: $(this).attr("title") != "" ? $(this).attr("title") : $(this).text(),
					tr: $(this).closest("tr"),
					duration: sl.durationToMs($(this).parent().nextAll(".duration").text())
				};

				if(cs) sm.stop(cs.sID);

				sl.currentSound = ss;
				sl.sounds.push(id);
				sl.sweep();

				ss.play();
				sl.syncVolume();
			}

			return false;
		});
	}

	this.log = function(msg) {
		if(typeof console != "undefined" &&
		   typeof console.log != "undefined") {
			console.log(msg);
		}
	}
}

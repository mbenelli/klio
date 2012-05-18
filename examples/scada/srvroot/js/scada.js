(function () {

  // Utils

  function pad(n) {
    return n < 10 ? '0' + n : (n).toString();
  }

  function month_name(idx, lang) {
    var lang = lang || 'en';
    var en = ['January', 'February', 'March', 'April', 'May', 'Juny',
	      'July', 'August', 'September', 'October', 'November', 'December'];
    var it = ['Gennaio', 'Febbraio', 'Marzo', 'Aprile', 'Maggio', 'Giugno',
	      'Luglio', 'Agosto', 'Settembre', 'Ottobre', 'Novembre', 'Dicembre'];
    return lang == 'it' ? it[idx-1] : en[idx-1];
  }

  // Jqplot default color
  var colors = ["#4bb2c5", "#c5b47f", "#EAA228", "#579575", "#839557", "#958c12",
    "#953579", "#4b5de4", "#d8b83f", "#ff5800", "#0085cc"];

  // Navigation

  var DEFAULT_PAGE = "overview";

  function show_page(page) {
    $(".page").css("visibility", "hidden");
    $(".page." + page).css("visibility", "visible");
  }

  function menu_init() {
    $("#menu li").each(function () {
      $(this).click(function () {
	show_page($(this).attr("class")); }); });
  }

  // Ajax

  function get_data(url, data, k) {
    $.ajax({
	url: url,
	type: "GET",
	data: data,
	dataType: "json",
	cache: false,
	error: function (jqXHR, textStatus, errorThrown) {
	  alert(errorThrown);
	},
	success: function (data, textStatus, jqXHR) {
	  k(data);
	}
      });
  }

  // Status

  function status() {
    get_data("/status", "", render_status);
  }

  function render_status(data) {
    $('#channels').empty();
    $.each(data.channels, function (k, v) {
      $('#channels').append('<tr><td>' + k + '</td><td>' + v + '</td></tr>');
    });
  }

  // Initializations

  function reset(p) {
    var p = p || DEFAULT_PAGE; 
    status();
    show_page(p);
  }

  $(document).ready(function () { reset('overview'); });

})();


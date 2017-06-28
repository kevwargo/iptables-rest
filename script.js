function iptRequest(url, method, data, selector) {
	$.ajax({
		url: url,
		method: method,
		data: data,
		success: function(data) {
			$(selector).html(renderjson(data));
		},
		error: function(jqXHR) {
			alert(jqXHR.responseJSON ? jqXHR.responseJSON.message : "Unknown error");
		}
	});
}

$(function() {
	$('#get-chains-button').click(function(event) {
		event.preventDefault();
		iptRequest('/chains?table=' + $('#table-name').val(), 'get', {}, '#chains-result');
	});
	$('#chain-create-button').click(function(event) {
		event.preventDefault();
		iptRequest('/chains', 'post', {table: $('#table-name').val(), chain: $('#chain-name').val()}, '#chain-create-result');
	});
	$('#rule-list-button').click(function(event) {
		event.preventDefault();
		iptRequest('/chains/' + $("#chain-name").val() + '/rules', 'get', [], '#rule-list-result');
	})
});

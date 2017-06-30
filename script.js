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

function recomputeMatchInputs() {
	$('.match').each(function(i) {
		$(this).data('num', i);
	});
	$('.match-input').each(function() {
		var num = $(this).closest('.match').data('num');
		switch ($(this).data('name')) {
		case 'name':
			$(this).attr('name', 'matches[' + num + '][name]');
			break;
		case 'opt-name':
			$(this).attr('name', 'matches[' + num + '][options][][name]');
			break;
		case 'opt-value':
			$(this).attr('name', 'matches[' + num + '][options][][value]');
			break;
		case 'opt-invert':
			$(this).attr('name', 'matches[' + num + '][options][][invert]');
			break;
		}
	});
}

function iptInit() {
	$("#tabset").tabs();
	
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
		iptRequest('/chains/' + $("#chain-name").val() + '/rules?table=' + $('#table-name').val(), 'get', [], '#rule-list-result');
	});

	$('#create-rule-form').submit(function(event) {
		event.preventDefault();
		var data = $(this).serializeObject();
		delete data['ignore'];
		iptRequest('/chains/' + $('#create-rule-chain-name').val() + '/rules?table=' + $('#create-rule-table-name').val(), 'post', JSON.stringify(data), '#create-rule-result');
	});

	$('.switch-parameter').change(function() {
		console.log(JSON.stringify({"checked": this.checked}));
		if (this.checked) {
			$($(this).data('selector'))
				.append($('<input name="params[' + $(this).data('name') + '][value]"/>'))
				.append('invert')
				.append($('<input type="checkbox" name="params[' + $(this).data('name') + '][invert]"/>'))
		} else {
			$($(this).data('selector')).empty();
		}
	});

	$('#add-match-button').click(function(event) {
		event.preventDefault();
		var matchNum = $(this).closest('.rule-spec').find('.match').length;
		console.log(matchNum);
		$(this).closest('div').before(
			$('<div class="match" data-num="' + matchNum + '">')
				.append('<input class="match-input" data-name="name"/>')
				.append($('<button type="button" class="del-match">Delete</button>'))
				.append('<div><button type="button" class="add-match-option-button">Add option</button></div>')
		);
		recomputeMatchInputs();
	});

	$(document).on('click', '.add-match-option-button', function(event) {
		event.preventDefault();
		$(event.target).closest('div').before(
			$('<div class="match-option">')
				.append($('<input class="match-input" data-name="opt-name"/>'))
				.append(': ')
				.append($('<input class="match-input" data-name="opt-value"/>'))
				.append($('<span class="invert-label">invert</span>'))
				.append($('<input class="match-input" type="checkbox" data-name="opt-invert"/>'))
				.append($('<button type="button" class="del-option">Delete</button>'))
		);
		recomputeMatchInputs();
	});

	$('#add-target-option-button').click(function(event) {
		event.preventDefault();
		$(this).closest('div').before(
			$('<div>')
				.append($('<input name="target[options][][name]"/>'))
				.append(': ')
				.append($('<input name="target[options][][value]"/>'))
				.append($('<button type="button" class="del-option">Delete</button>'))
		);
	});

	$(document).on('click', '.del-option', function(event) {
		event.preventDefault();
		$(event.target).closest('div').remove();
	});
	
	$(document).on('click', '.del-match', function(event) {
		event.preventDefault();
		$(event.target).closest('div').remove();
		recomputeMatchInputs();
	});

}

$(iptInit);

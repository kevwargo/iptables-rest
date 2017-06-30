function iptRequest(url, method, data, selector, transform) {
	$.ajax({
		url: url,
		method: method,
		data: data,
		success: function(data) {
			if (selector) {
				$(selector).html((transform || renderjson)(data));
			}
		},
		error: function(jqXHR, text, status) {
			alert(jqXHR.responseJSON ? jqXHR.responseJSON.message : status);
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

function getChainName($context) {
	return $context.closest('.tab').find('.chain-name').val();
}

function getTableName($context) {
	return $context.closest('.tab').find('.table-name').val();
}

function refreshRules($context) {
	return iptRequest(
		'/chains/' + getChainName($context) + '/rules?table=' + getTableName($context),
		'get',
		[],
		'#rule-list-result',
		function (data) {
			return $('<table>')
				.append($('<thead>').html(
					'<tr><th>#</th><th>Parameters</th><th>Matches</th><th>Target</th></tr>'
				))
				.append(
					$('<tbody>')
						.append(
							$(data.map(function(rule, num) {
								return '<tr>'
									+ '<td>' + (num + 1) + '</td>'
									+ '<td>' + JSON.stringify(rule.params) + '</td>'
									+ '<td>' + JSON.stringify(rule.matches) + '</td>'
									+ '<td>' + JSON.stringify(rule.target) + '</td>'
									+ '<td><button type="button" class="del-rule">Del</button></td>'
									+ '</tr>';
							}).join(''))
						)
				)
		}
	);
}

function addMatch() {
	var matchNum = $('#matches-container').find('.match').length;
	return $('#matches-list').append(
		$('<div class="match" data-num="' + matchNum + '">')
			.append('<input class="match-input" data-name="name"/>')
			.append($('<button type="button" class="del-match">Delete</button>'))
			.append($('<div class="match-option-list">'))
			.append('<div><button type="button" class="add-match-option-button">Add option</button></div>')
	);
}

function addMatchOption(matchNum) {
	return $('.match[data-num="' + matchNum + '"] .match-option-list').append(
		$('<div class="match-option">')
			.append($('<input class="match-input" data-name="opt-name"/>'))
			.append(': ')
			.append($('<input class="match-input" data-name="opt-value"/>'))
			.append($('<span class="invert-label">invert</span>'))
			.append($('<input class="match-input" type="checkbox" data-name="opt-invert"/>'))
			.append($('<button type="button" class="del-option">Delete</button>'))
	);
}

function updateParamsCheckboxes() {
	if (this.checked) {
		$($(this).data('selector'))
			.append($('<input name="params[' + $(this).data('name') + '][value]"/>'))
			.append('invert')
			.append($('<input type="checkbox" name="params[' + $(this).data('name') + '][invert]"/>'))
	} else {
		$($(this).data('selector')).empty();
	}
}

function iptInit() {
	$("#tabset").tabs();

	$('.switch-parameter').each(updateParamsCheckboxes);

	$('#get-chains-button').click(function(event) {
		event.preventDefault();
		iptRequest('/chains?table=' + getTableName($(this)), 'get', {}, '#chains-result');
	});
	$('#chain-create-button').click(function(event) {
		event.preventDefault();
		iptRequest(
			'/chains',
			'post',
			{table: getTableName($(this)), chain: getChainName($(this))},
			'#chain-create-result');
	});
	$('#rule-list-button').click(function(event) {
		event.preventDefault();
		refreshRules($(this));
	});

	$('#create-rule-form').submit(function(event) {
		event.preventDefault();
		var data = $(this).serializeObject();
		delete data['ignore'];
		iptRequest('/chains/' + getChainName($(this)) + '/rules?table=' + getTableName($(this)), 'post', JSON.stringify(data), '#create-rule-result');
	});

	$('.switch-parameter').change(updateParamsCheckboxes);

	$('#add-match-button').click(function(event) {
		event.preventDefault();
		addMatch();
		recomputeMatchInputs();
	});

	$(document).on('click', '.add-match-option-button', function(event) {
		event.preventDefault();
		addMatchOption($(this).closest('.match').data('num'));
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

	$(document).on('click', '.del-rule', function(event) {
		var $this = $(this);
		event.preventDefault();
		$.ajax({
			url: '/chains/' + getChainName($this) + '/rules/'
				+ $this.closest('tr').children('td:first-child').text()
				+ '?table=' + getTableName($this),
			method: 'delete',
			success: function() {
				refreshRules($this);
			},
			error:  function(jqXHR, text, status) {
				alert(jqXHR.responseJSON ? jqXHR.responseJSON.message : status);
			}
		});
	});

}

$(iptInit);

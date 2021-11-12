let g:projectionist_heuristics = {
	\	'*': {
	\		'*.c': {
	\			'alternate': [
	\				'include/{}.h',
	\				'{}.h'
	\			],
	\			'type': 'source'
	\		},
	\		'*.h': {
	\			'alternate': [
	\				'../{}.c',
	\				'{}.c',
	\			],
	\			'type': 'header'
	\		},
	\	},
	\}

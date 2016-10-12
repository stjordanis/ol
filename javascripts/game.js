function board_size(v)
{
	return radio('board_size', v);
}

function players_adjacency(v)
{
	return radio('players_adjacency', v);
}

function worlds_density(v)
{
	return radio('worlds_density', v);
}

function options(v)
{
	return check('options', v);
}

function myrace()
{
	return parseInt(game.elements.myrace.value);
}

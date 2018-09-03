$(function(){
    $('#lookup-molecule').click(moleculeLookup);
    $('#add-molecule').click(moleculeAdd);
    $('#lookup-catalyst').click(catalystLookup);
    $('#add-catalyst').click(catalystAdd);
    $('#lookup-reaction').click(reactionLookup);
    $('#add-reaction').click(reactionAdd);
    $('#add-reagent').click(reagentAdd);
    $('#add-product').click(productAdd);
    $('#add-reaction-catalyst').click(reactionCatalystAdd);
    $('#find-path').click(findPath);
});

function moleculeLookup() {
    var idStr = $('#molecule-id').val();
    var id = parseInt(idStr);
    if (idStr === '*') {
        id = '';
    } else if (Number.isNaN(id)) {
        setMoleculeResult(errorText("Please, specify id or * for all"));
        return;
    }
    $.ajax({
        url: './molecule/' + id,
        dataType: 'text',
        success: function(data) {
            setMoleculeResult(data.replace(/\;/g, '<br/>'));
        },
        error: function(resp) {
            setMoleculeResult(errorText(resp.responseText));
        }
    });
}

function moleculeAdd() {
    var id = parseInt($('#molecule-id').val());
    var name = $('#molecule-name').val().trim();
    var smiles = $('#molecule-smiles').val().trim();
    if (Number.isNaN(id) || name === '' || smiles === '') {
        setMoleculeResult(errorText('Please, fill all 3 fields'));
        return;
    }
    $.ajax({
        method: 'POST',
        url: './molecule/' + id,
        data: smiles + ' ' + name,
        dataType: 'text',
        success: function(data) {
            if (data.trim() !== 'ok') {
                data = errorText(data);
            }
            setMoleculeResult(data);
        }
    });
}

function setMoleculeResult(res) {
    $('#molecule-result').html(res);
}

function errorText(str) {
    return '<span class="error">' + str + '</span>';
}

function catalystLookup() {
    var idStr = $('#catalyst-id').val();
    var id = parseInt(idStr);
    if (idStr === '*') {
        id = '';
    } else if (Number.isNaN(id)) {
        setCatalystResult(errorText("Please, specify id or * for all"));
        return;
    }
    $.ajax({
        url: './catalyst/' + id,
        dataType: 'text',
        success: function(data) {
            setCatalystResult(data.replace(/\;/g, '<br/>'));
        },
        error: function(resp) {
            setCatalystResult(errorText(resp.responseText));
        }
    });
}

function catalystAdd() {
    var id = parseInt($('#catalyst-id').val());
    var name = $('#catalyst-name').val().trim();
    var smiles = $('#catalyst-smiles').val().trim();
    if (Number.isNaN(id) || smiles === '') {
        setCatalystResult(errorText('Please, fill ID and SMILES'));
        return;
    }
    $.ajax({
        method: 'POST',
        url: './catalyst/' + id,
        data: smiles + ' ' + name,
        dataType: 'text',
        success: function(data) {
            if (data.trim() !== 'ok') {
                data = errorText(data);
            }
            setCatalystResult(data);
        }
    });
}

function setCatalystResult(res) {
    $('#catalyst-result').html(res);
}

function reactionLookup() {
    var idStr = $('#reaction-id').val();
    var id = parseInt(idStr);
    if (idStr === '*') {
        id = '';
    } else if (Number.isNaN(id)) {
        setReactionResult(errorText("Please, specify id or * for all"));
        return;
    }
    $.ajax({
        url: './reaction/' + id,
        dataType: 'text',
        success: function(data) {
            setReactionResult(data.replace(/\;/g, '<br/>'));
        },
        error: function(resp) {
            setReactionResult(errorText(resp.responseText));
        }
    });
}

function reactionAdd() {
    var id = parseInt($('#reaction-id').val());
    var name = $('#reaction-name').val().trim();
    if (Number.isNaN(id) || name === '') {
        setReactionResult(errorText('Please, fill ID and name'));
        return;
    }
    $.ajax({
        method: 'POST',
        url: './reaction/' + id,
        data: name,
        dataType: 'text',
        success: function(data) {
            if (data.trim() !== 'ok') {
                data = errorText(data);
            }
            setReactionResult(data);
        }
    });
}

function reagentAdd() {
    var id = parseInt($('#reaction-id').val());
    var mid = $('#reaction-reagent-id').val().trim();
    var qty = $('#reaction-reagent-amount').val().trim();
    if (Number.isNaN(id) || Number.isNaN(mid) || Number.isNaN(qty)) {
        setReactionResult(errorText('Please, fill reaction ID, reagent molecule ID and amount'));
        return;
    }
    $.ajax({
        method: 'POST',
        url: './reagent_in',
        data: id + ' ' + mid + ' ' + qty,
        dataType: 'text',
        success: function(data) {
            if (data.trim() !== 'ok') {
                data = errorText(data);
            }
            setReactionResult(data);
        }
    });
}

function productAdd() {
    var id = parseInt($('#reaction-id').val());
    var mid = $('#reaction-product-id').val().trim();
    var qty = $('#reaction-product-amount').val().trim();
    if (Number.isNaN(id) || Number.isNaN(mid) || Number.isNaN(qty)) {
        setReactionResult(errorText('Please, fill reaction ID, product molecule ID and amount'));
        return;
    }
    $.ajax({
        method: 'POST',
        url: './product_from',
        data: mid + ' ' + id + ' ' + qty,
        dataType: 'text',
        success: function(data) {
            if (data.trim() !== 'ok') {
                data = errorText(data);
            }
            setReactionResult(data);
        }
    });
}

function reactionCatalystAdd() {
    var id = parseInt($('#reaction-id').val());
    var mid = $('#reaction-catalyst-id').val().trim();
    var tmp = $('#reaction-catalyst-tmp').val().trim();
    var prs = $('#reaction-catalyst-prs').val().trim();
    if (Number.isNaN(id) || Number.isNaN(mid) || Number.isNaN(tmp) || Number.isNaN(prs)) {
        setReactionResult(errorText('Please, fill reaction ID, catalyst ID, temperature and pressure'));
        return;
    }
    $.ajax({
        method: 'POST',
        url: './accelerate',
        data: id + ' ' + mid + ' ' + tmp + ' ' + prs,
        dataType: 'text',
        success: function(data) {
            if (data.trim() !== 'ok') {
                data = errorText(data);
            }
            setReactionResult(data);
        }
    });
}

function setReactionResult(res) {
    $('#reaction-result').html(res.replace(/\n/g, '<br/>'));
}

function findPath() {
    var src = parseInt($('#path-start').val());
    var dst = parseInt($('#path-end').val());
    if (Number.isNaN(src) || Number.isNaN(dst)) {
        setPathResult(errorText('Please, fill molecule IDs for start and end of the path'));
        return;
    }
    $.ajax({
        url: './find_path/' + src + '/' + dst,
        dataType: 'text',
        success: function(data) {
            setPathResult(data.replace(/\n/g, '<br/>'));
        },
        error: function(resp) {
            setPathResult(errorText(resp.responseText));
        }
    });
}

function setPathResult(res) {
    $('#path-result').html(res.replace(/\n/g, '<br/>'));
}


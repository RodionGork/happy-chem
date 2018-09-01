$(function(){
    $('#lookup-molecule').click(moleculeLookup);
    $('#add-molecule').click(moleculeAdd);
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


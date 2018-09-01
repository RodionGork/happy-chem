$(function(){
    $('#lookup-molecule').click(moleculeLookup);
});

function moleculeLookup() {
    var id = parseInt($('#molecule-id').val());
    if (Number.isNaN(id)) {
        id = '';
    }
    $.ajax({
        url: './molecule/' + id,
        success: function(data) {
            $('#molecule-result').html(data.replace(/\;/g, '<br/>'));
        },
        error: function(resp) {
            $('#molecule-result').html(resp.responseText);
        },
        dataType: 'text'
    });
}

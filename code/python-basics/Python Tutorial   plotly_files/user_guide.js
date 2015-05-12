(function () {
  $(function() {
    init();
    var cursyntax = 'vanilla';

    function init() {
      highlightJSinit();
    }

    function highlightJSinit() {
      $('code').each(function(i, e) {
        hljs.highlightBlock(e);
      });
      $('pre').each(function(i, e) {
          if (!$(e).parent().hasClass('output_subarea')) {
            hljs.highlightBlock(e);
          }
      });
    }


    $('#display_key').click(function() {
      requireAuth({trigger: 'USER'}).done();
    });

  })
})();

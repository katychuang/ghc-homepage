$(function(){
  $('h1,h2,h3,h4,h5,h6').each(function(){
    var h = $(this);
    h.find('a').each(function(){
      var a = $(this);
      var href = a.attr('id');
      if (href && !a.text()) {
        h.prepend($("<a>").attr('href','#' + href).addClass('section-sign').text('§'));
        return false;
      }
    });
  });

  $('p').each(function(){
    var h = $(this);
    h.find('a').each(function(){
      var a = $(this);
      var href = a.attr('id');
      if (href && !a.text()) {
        h.append($("<a>").attr('href','#' + href).addClass('para-sign').text('¶'));
        return false;
      }
    });
  });

  hljs.configure({useBR: true});
  $('.verbatim').each(function(i, block) {
    hljs.highlightBlock(block);
    block.innerHTML =
      block.innerHTML.replace(/&nbsp;(::|=|\-&gt;)&nbsp;/g,
                              '<span class="hljs-keyglyph">&nbsp;$1&nbsp;</span>');
  });
});

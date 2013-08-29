jQuery(function() {
  $('#navbar li a').click(function() {
    $('html, body').animate({
      scrollTop: $($(this).attr('href')).offset().top - $('#navbar').height()
    }, 300);

    return false;
  });
  
  $('#navbar a.brand').click(function() {
    $('html, body').animate({
      scrollTop: 0
    }, 300);
    
    return false;
  });
  
  $('#intro').css('top', $('#navbar').outerHeight());
  $('#sections').css('margin-top', $('#navbar').outerHeight() + $('#intro').outerHeight());
});

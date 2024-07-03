document.getElementById('menuIcon').addEventListener('click', function() {
  var menu = document.getElementById('menu');
  menu.style.display = (menu.style.display === 'block') ? 'none' : 'block';
});

var menuItems = document.querySelectorAll('.menuItem');

menuItems.forEach(function(item) {
  var submenu = item.nextElementSibling;

  // Show submenu on mouseover
  item.addEventListener('mouseover', function() {
    submenu.style.display = 'block';
  });

  // Hide submenu on mouseout only if the mouse is not over the submenu
  item.addEventListener('mouseout', function(event) {
    if (!isMouseOverElement(event, submenu)) {
      submenu.style.display = 'none';
    }
  });

  // Hide submenu on mouseout only if the mouse is not over the menu item or the submenu
  submenu.addEventListener('mouseout', function(event) {
    if (!isMouseOverElement(event, item) && !isMouseOverElement(event, submenu)) {
      submenu.style.display = 'none';
    }
  });
});

// Utility function to check if the mouse is over an element
function isMouseOverElement(event, element) {
  var rect = element.getBoundingClientRect();
  return (
    event.clientX >= rect.left &&
    event.clientX <= rect.right &&
    event.clientY >= rect.top &&
    event.clientY <= rect.bottom
  );
}
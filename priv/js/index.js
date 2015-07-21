// Create a connection to http://localhost:9999/echo
Var sock = new SockJS('http://www.localhost-443.com/echo');

// Open the connection
sock.onopen = function() {
    console.log('open');
};

// On connection close
sock.onclose = function() {
    console.log('close');
};

// On receive message from server
sock.onmessage = function(e) {
  // Get the content
    var content = JSON.parse(e.data);

  // Append the text to text area (using jQuery)
    $('#chat-content').val(function(i, text){
        return text + 'User ' + content.username + ': ' + content.message + '\n';
    });
  
};

// Function for sending the message to server
function sendMessage(){
  // Get the content from the textbox
    var message = $('#message').val();
    var username = $('#username').val();

  // The object to send
    var send = {
        message: message,
        username: username
    };

  // Send it now
    sock.send(JSON.stringify(send));
}
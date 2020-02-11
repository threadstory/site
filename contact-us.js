var btn = document.getElementById("send-enquiry");

var nam =  document.getElementById("name");
var phone = document.getElementById("phone");
var email =  document.getElementById("email");
var product =  document.getElementById("product");
var number =  document.getElementById("quantity");

// var post_url = "http://localhost:9090/contact-us";
var post_url = "https://apis.threadstory.in/contact-us";

var error_message = document.getElementById("error-message");
var success_message = document.getElementById("success-message");

var error_txt = document.getElementById("error-text");

btn.onclick = function (e) {
    e.preventDefault();

    var data = {
	name: nam.value,
	phone: phone.value,
	email: email.value,
	product: product.value,
	quantity: quantity.value
    }

    
    if ((data.name && data.name.length > 0) &&
	((data.phone && data.phone.length > 0 && data.phone.length < 20) ||
	 (data.email && data.email.length > 0))) {

	var xhr = new XMLHttpRequest();
	xhr.onreadystatechange = function () {
	    if (this.readyState != 4) return;

	    if (this.status == 200) {
		var data = JSON.parse(this.responseText);

		console.log("success");
		// we get the returned data
		console.log(data);

		success_message.classList.remove("hidden");
		error_message.classList.add("hidden");
	    } else {
		error_txt.innerHTML = "Could not record your query. Please call us @ +91-9090701366";
		success_message.classList.add("hidden");
		error_message.classList.remove("hidden");
	    }



	    // end of state change: it can be after some time (async)
	};
	xhr.open("POST", post_url, true);
	xhr.setRequestHeader('Content-Type', 'application/json');
	xhr.send(JSON.stringify({
	    info: data
	}));
	
    } else {
	error_txt.innerHTML = "Please provide Name and email / phone-number";
	success_message.classList.add("hidden");
	error_message.classList.remove("hidden");
	console.log("Incomplete data");
    }

    
}

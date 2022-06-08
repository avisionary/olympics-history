// When the user scrolls down 20px from the top of the document, slide down the navbar
window.onscroll = function () {
    scrollFunction();
};

function scrollFunction() {
    if (
        document.body.scrollTop >= 50 ||
        document.documentElement.scrollTop >= 50
    ) {
        document.getElementById("navbar").style.top = "0";
    } else {
        document.getElementById("navbar").style.top = "-50";
    }
    //New code added
    if (
        document.body.scrollTop > 20 ||
        document.documentElement.scrollTop > 20
    ) {
        document.getElementById('scroller').style.display = 'none';
    } else {
        document.getElementById("scroller").style.display = "block";
    }
}


function scrollFunction2() {
    if (
        document.body.scrollTop >= 0 ||
        document.documentElement.scrollTop >= 0
    ) {
        document.getElementById("bg2").style.display = "none";
        document.getElementById("bg").style.display = "block";
    } else {
        document.getElementById("bg2").style.display = "block";
        document.getElementById("bg").style.display = "none";
    }
}
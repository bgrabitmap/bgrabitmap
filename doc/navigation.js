if (typeof mainNav === "undefined") {
	mainNav = document.getElementsByClassName("sections")[0];
	if (!mainNav)
	{
		mainNav = document.getElementsByTagName("h1")[0];
		mainNav.style.backgroundColor = "white";
		mainNav.style.width = "98%";
	}

	if (mainNav) {
		mainNav.style.position = "fixed";
		mainNav.style.top = 0;

		const leftNavigation = document.getElementsByClassName("navigation")[0];
		if (leftNavigation) {
			const menuToggle = document.createElement("div");
			menuToggle.className = "menu_toggle";
			mainNav.insertBefore(menuToggle, mainNav.firstChild);
			menuToggle.addEventListener('click', function(e) {
				e.preventDefault();
				if (leftNavigation.style.display == "none") {
					leftNavigation.style.animation = "fadeIn 0.5s ease-out forwards";
					leftNavigation.style.position = "fixed";
					leftNavigation.style.margin = "0";
					leftNavigation.style.left = menuToggle.offsetLeft + "px";
					leftNavigation.style.top = (menuToggle.offsetTop + menuToggle.offsetHeight) + "px";
					leftNavigation.style.display = "block";					
				} else {
					leftNavigation.style.display = "none";
				}
			});
			
			leftNavigation.style.display = "none";
			leftNavigation.style.zIndex = "1000";
			
			const content = document.getElementsByClassName("content")[0];
			content.style.paddingTop = (mainNav.offsetHeight + 4) + "px";
			content.style.margin = "0";
		}
	}
}

window.onload = function() {		
	window.scrollBy(0, -mainNav.offsetHeight-8);

	const title = document.getElementsByTagName("h1")[0];
	const isUnit = title && title.innerText.startsWith("Unit ");
	const descriptionLocation = document.getElementById("PasDoc-Description");
	if (title && descriptionLocation) title.appendChild(descriptionLocation);
	
	const descriptionTitle = document.getElementsByClassName("description")[0];
	if (descriptionTitle) {		
		const descriptionContent = [];
		let curNode = descriptionTitle.nextSibling;
		descriptionTitle.remove();
		while (curNode && curNode.nodeName != "SPAN") {
			descriptionContent.push(curNode);
			curNode = curNode.nextSibling;
		}
		const beforeNode = title.nextSibling;
		for (const node of descriptionContent) {
			title.parentNode.insertBefore(node, beforeNode);
		}
	}
	
	if (!isUnit) {
		const inUnitTitle = document.getElementsByClassName("unit")[0];
		if (inUnitTitle) inUnitTitle.remove();
		const unitLink = document.getElementsByClassName("unitlink")[0];
		if (unitLink) {
			const a = unitLink.getElementsByTagName("a")[0];
			if (a) {
				a.innerHTML = "🗎&nbsp;" + a.innerHTML;
			}
			unitLink.style.float = "right";
		}
	}
	
	// DESPLAZAMIENTO SMOOTH SCROLL
    const easeInCubic = function(t) { return 0.5 - Math.cos(t * Math.PI)/2 }
    const scrollElems = document.getElementsByClassName('section');

    const scrollToElem = (start, stamp, duration, scrollEndElemTop, startScrollOffset) => {

        const runtime = stamp - start;
        let progress = runtime / duration;
        const ease = easeInCubic(progress);

        progress = Math.min(progress, 1);

        const newScrollOffset = startScrollOffset + (scrollEndElemTop * ease);
        window.scroll(0, startScrollOffset + (scrollEndElemTop * ease));

        if (runtime < duration) {
            requestAnimationFrame((timestamp) => {
                const stamp = new Date().getTime();
                scrollToElem(start, stamp, duration, scrollEndElemTop, startScrollOffset);
            })
        }
    }

    for (let i = 0; i < scrollElems.length; i++) {
        const elem = scrollElems[i];

        elem.addEventListener('click', function(e) {
            e.preventDefault();
			const leftNavigation = document.getElementsByClassName("navigation")[0];
			if (leftNavigation)
				leftNavigation.style.display = "none";
			
            const scrollElemId = e.target.href.split('#')[1];
            const scrollEndElem = document.getElementById(scrollElemId);

            const anim = requestAnimationFrame(() => {
                const stamp = new Date().getTime();
                const duration = 500;
                const start = stamp;

                const startScrollOffset = window.pageYOffset;

                const scrollEndElemTop = scrollEndElem.getBoundingClientRect().top - mainNav.offsetHeight;

                scrollToElem(start, stamp, duration, scrollEndElemTop, startScrollOffset);
            })
        })
    }
}

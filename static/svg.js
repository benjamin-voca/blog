document.addEventListener('DOMContentLoaded', () => {
    // Array of paths for the external SVG files
    const svgPaths = [
        './hsl.svg',
        './nix.svg'
    ];

    // Function to fetch one SVG file
    const fetchSVG = (path) => {
        return fetch(path)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`Error fetching ${path}: ${response.statusText}`);
                }
                return response.text();
            });
    };

    // Fetch all SVGs in parallel
    Promise.all(svgPaths.map(fetchSVG))
        .then(svgContents => {
            // Wrap each fetched SVG in a container div with class "svg-slide"
            const slidesHTML = svgContents.map(content => `<div class="svg-slide">${content}</div>`);
            const container = document.getElementById('svg-container');
            container.innerHTML = slidesHTML.join('');

            // Select all slide elements
            const slides = document.querySelectorAll('.svg-slide');
            let current = 0;

            // Immediately add "active" to the first slide's SVG element after a delay
            setTimeout(() => {
                const firstSVG = slides[current].querySelector('svg');
                if (firstSVG) {
                    firstSVG.classList.add('active');
                }
            }, 10); // Adjust delay as needed

            // Function to switch slides with a delay between removal and addition
            const switchSlide = () => {
                const currentSVG = slides[current].querySelector('svg');
                if (currentSVG) {
                    currentSVG.classList.remove('active');
                }
                // After a short delay (e.g., 300ms), add the active class to the next slide
                setTimeout(() => {
                    currentSVG.classList.add("hidden");
                    current = (current + 1) % slides.length;
                    const nextSVG = slides[current].querySelector('svg');
                    if (nextSVG) {
                        currentSVG.classList.remove("hidden");
                        nextSVG.classList.add('active');
                    }
                }, 3000);
            };

            // Cycle through the slides every 3 seconds
            setInterval(switchSlide, (4000 + (slides.length * 3000)));
        })
        .catch(err => {
            console.error('Error loading SVGs:', err);
        });
});

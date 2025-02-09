// import { animate } from "motion"

// Global object to store preloaded article content.
window.preloadedArticles = {};
// Preload each article file on page load.
document.addEventListener('DOMContentLoaded', () => {
    const articles = document.querySelectorAll('.mini-article');
    const preloadPromises = [];

    articles.forEach(article => {
        const articleName = article.dataset.articleName;
        const url = `/blogs/${articleName}.html`;

        const promise = fetch(url)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`Failed to load ${url}: ${response.statusText}`);
                }
                return response.text();
            })
            .then(html => {
                window.preloadedArticles[articleName] = html;
            })
            .catch(error => {
                console.error('Error preloading article', articleName, error);
            });

        preloadPromises.push(promise);
    });

    Promise.all(preloadPromises).then(() => {
        console.log("All articles preloaded:", window.preloadedArticles);
    });

    // Use IntersectionObserver to add 'in-view' class for scale animation.
    const observer = new IntersectionObserver((entries, observer) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('in-view');
                observer.unobserve(entry.target);
            }
        });
    }, { threshold: 0.5 });

    articles.forEach(article => {
        observer.observe(article);
    });
});

// When a mini-article is clicked, expand it to show the full article.
function openArticle(event) {
    const articleElem = event.currentTarget;
    // Prevent re-opening if already expanded.
    if (articleElem.classList.contains('expanded')) return;

    const articleName = articleElem.dataset.articleName;
    const articleContent = window.preloadedArticles[articleName];
    if (!articleContent) {
        console.error(`Article content not found for "${articleName}"`);
        return;
    }

    // Create a container for the preloaded article content.
    const contentContainer = document.createElement('div');
    contentContainer.className = 'expanded-content';
    contentContainer.innerHTML = articleContent;

    // Create a close button to collapse the view.
    const closeButton = document.createElement('button');
    closeButton.className = 'close-btn';
    closeButton.textContent = 'Close';
    closeButton.addEventListener('click', (e) => {
        e.stopPropagation(); // Prevent the click from triggering re-expansion.
        if (articleElem.contains(contentContainer)) {
            articleElem.removeChild(contentContainer);
        }
        articleElem.classList.remove('expanded');
    });

    contentContainer.appendChild(closeButton);
    articleElem.appendChild(contentContainer);

    // Force reflow to ensure the transition triggers.
    void articleElem.offsetWidth;
    articleElem.classList.add('expanded');
}

window.openArticle = openArticle;

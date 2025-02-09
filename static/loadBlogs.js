async function fetchBlogList() {
    try {
        const response = await fetch("blogs/list.json"); // Fetch blog list
        if (!response.ok) throw new Error("Could not retrieve blog list.");

        const blogFiles = await response.json(); // Expecting an array of filenames
        const blogList = document.getElementById("blog-list");

        blogFiles.forEach(file => {
            const title = file.replace(".html", ""); // Extract title from filename
            const listItem = document.createElement("li");
            const button = document.createElement("button");

            button.textContent = title;
            button.setAttribute("data-blog", file);
            button.addEventListener("click", () => loadBlogContent(file));

            listItem.appendChild(button);
            blogList.appendChild(listItem);
        });
    } catch (error) {
        console.error(error);
    }
}

async function loadBlogContent(filename) {
    try {
        const response = await fetch(`blogs/${filename}`);
        if (!response.ok) throw new Error("Blog not found");

        const content = await response.text();
        document.getElementById("blog-title").textContent = filename.replace(".html", "");

        // Wrap content in a styled article
        document.getElementById("blog-content").innerHTML = `
            <article class="blog-post">
                ${content}
            </article>
        `;

        document.getElementById("blog-modal").showModal();
    } catch (error) {
        document.getElementById("blog-title").textContent = "Error";
        document.getElementById("blog-content").textContent = "<p>Could not load blog content.</p>";
    }
}


// Load blog list on page load
fetchBlogList();

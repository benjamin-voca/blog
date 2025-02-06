const modal = document.getElementById("blog-modal");
const closeModal = document.getElementById("close-modal");

document.getElementById("blog-list").addEventListener("click", (event) => {
    if (event.target.tagName === "BUTTON") {
        const blogId = event.target.getAttribute("data-blog");
        loadBlogContent(blogId);
        modal.showModal(); // Opens the modal
    }
});

closeModal.addEventListener("click", () => {
    modal.classList.add("closing"); // Start close animation
    setTimeout(() => {
        modal.close();
        modal.classList.remove("closing"); // Reset class after closing
    }, 300); // Match CSS transition duration
});

// Prevent modal from closing immediately when clicking outside
modal.addEventListener("cancel", (event) => {
    event.preventDefault(); // Prevent default close
    closeModal.click(); // Trigger close button behavior
});

function loadBlogContent(id) {
    const blogs = {
        1: { title: "Blog Post 1", content: "This is the content of blog post 1." },
        2: { title: "Blog Post 2", content: "This is the content of blog post 2." }
    };

    document.getElementById("blog-title").textContent = blogs[id].title;
    document.getElementById("blog-content").textContent = blogs[id].content;
}

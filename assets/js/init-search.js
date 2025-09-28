window.addEventListener("DOMContentLoaded", (event) => {
    new PagefindUI({
        element: "#search",
        showSubResults: true,
        showImages: false,
        sort: { date: "desc" },
    });
});

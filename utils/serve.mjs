#!node
import fs from "fs";
// eslint-disable-next-line @typescript-eslint/no-unused-vars
import http, { Server } from "http";

/** @type {(path: string) => string} */
const parpath = (path) => `/${path.replace(/\/?[^/]+\/?$/, "")}`;

/**
 * @param res {http.ServerResponse} The response object
 * @param path {string} The path to the file
 */
function serveFile(res, path) {
    if (fs.existsSync(path)) {
        fs.createReadStream(path).pipe(res);
    } else if (fs.existsSync("404.html")) {
        fs.createReadStream("404.html").pipe(res);
    } else {
        res.write(
            `<h1>404 not found, path: ${path}</h1><br/>`
                + `<a href="/">Back to root</a><br/>`
                + `<a href="${parpath(path)}">Back to upper dir</a>`,
        );
        res.end();
    }
}

/**
 * @param res {http.ServerResponse} The response object
 * @param path {string} The path to the directory
 */
function serveDir(res, path) {
    res.write(`<h1>Directory listing of /${path}</h1>`);
    res.write(
        `<input type="checkbox" name="isRaw" onchange="action('raw')" /> `
            + `<label for="isRaw">Get raw content</label>`,
    );
    res.write(`<hr/>`);
    res.write(`<a href="${parpath(path)}">../</a><br/>`);
    const dir = fs.opendirSync(`./${path}`, {});
    for (let f = dir.readSync(); f; f = dir.readSync()) {
        res.write(
            `<a href="/${path ? `${path}/` : ""}${f.name}" rel="keep-params">${
                f.name + (f.isDirectory() ? "/" : "")
            }</a>${f.isSymbolicLink() ? ` -> ${fs.readlinkSync(path + f.name)}` : ""} <br/>`,
        );
    }
    dir.closeSync();
    res.write(
        `<hr/><label for="filesel">Select file to upload:</label>`
            + `<input type="file" id="filesel" /><br/>`
            + `<button onclick="action('upload')">Upload to current directory</button></form>`,
    );
    /** @type {(type: "upload" | "raw") => Promise<void>} */
    async function action(type) {
        if (type === "upload") {
            const fsel = document.getElementById("filesel");
            if (!(fsel instanceof HTMLInputElement)) throw new Error("Type mismatch");
            const file = fsel.files?.[0];
            if (!file) throw new Error("Unexpected null");
            await fetch(`${document.URL}?upload=${file.name}`, {
                method: "POST",
                body: await file.arrayBuffer(),
            });
        } else if (type === "raw") {
            const url = new URL(document.URL);
            const rawbox = document.getElementsByName("isRaw")[0];
            if (!(rawbox instanceof HTMLInputElement)) throw new Error("Type mismatch");
            if (rawbox.checked) {
                url.searchParams.set("raw", "");
            } else {
                url.searchParams.delete("raw");
            }
            window.history.replaceState("", "", url.toString());
            const event = new CustomEvent("toggle-raw", { detail: rawbox.checked });
            for (const link of document.getElementsByTagName("a")) {
                link.dispatchEvent(event);
            }
        }
    }
    async function init() {
        const rawbox = document.getElementsByName("isRaw")[0];
        if (!(rawbox instanceof HTMLInputElement)) throw new Error("Type mismatch");
        rawbox.checked = new URL(document.URL).searchParams.get("raw") !== null;
        for (const link of document.getElementsByTagName("a")) {
            link.addEventListener("toggle-raw", (e) => {
                if (!(e instanceof CustomEvent && e.target instanceof HTMLAnchorElement)) {
                    throw new Error("Type mismatch");
                }
                const url = new URL(e.target.href);
                if (e.detail) {
                    url.searchParams.set("raw", "");
                } else {
                    url.searchParams.delete("raw");
                }
                e.target.href = url.toString();
            });
        }
        await action("raw");
    }
    res.write(`<script>${init.toString()}; ${action.toString()}; init();</script>`);
    res.end();
}

/**
 * @param req {http.IncomingMessage} The request
 * @param path {string} The path to the dir
 * @param fname {string} The preferred file name
 */
function doUpload(req, path, fname) {
    let savename = `./${path}/${fname}`;
    const parts = fname.split(".");
    const [pre, post] = [parts.slice(0, parts.length - 1).join("."), parts[parts.length - 1]];
    for (let i = 1; fs.existsSync(savename); i += 1) savename = `./${path}/${pre} (${i}).${post}`;
    req.pipe(fs.createWriteStream(savename));
}

const serv = new Server();
serv.on("request", (req, res) => {
    const url = new URL(req.url || "", `http://${req.headers.host || "localhost"}`);
    const path = url.pathname.slice(1);
    if (path === undefined) {
        // eslint-disable-next-line no-console
        console.error("Path of request undefined:", req);
        res.writeHead(500);
        res.end();
        return;
    }
    if (url.searchParams.get("upload") !== null && req.method?.toUpperCase() === "POST") {
        const fname = url.searchParams.get("upload") || "";
        if (fname.includes("/")) {
            res.writeHead(400).end("Error: there can't be '/' in file names");
        } else {
            doUpload(req, path, fname);
            res.writeHead(200).end();
        }
        return;
    }
    if (path.length === 0 || (fs.existsSync(path) && fs.statSync(path).isDirectory())) {
        res.writeHead(200);
        if (url.searchParams.get("raw") === null && fs.existsSync(`${path}/index.html`)) {
            serveFile(res, `${path}/index.html`);
        } else {
            serveDir(res, path);
        }
    } else {
        /** @type {http.OutgoingHttpHeaders} */
        const headers = {};
        if (url.searchParams.get("raw") !== null) headers["Content-Type"] = "text/plain";
        res.writeHead(200, headers);
        serveFile(res, path);
    }
});

serv.listen(8000);

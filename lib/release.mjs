import * as Core from "@actions/core";
import * as Github from "@actions/github";

/**
 * @type {(key: string) => { ok: true, value: string } | { ok: false, error: string }}
 */
const requiredParam = (key) => {
	const value = process.env[key];
	if (value === undefined || value === "") {
		Core.setFailed(`Please add ${key} to env`);
		return null;
	}
	return value;
};

const optionalParam = (key) => {
	const value = process.env[key];
	if (value === undefined || value === "") {
		return null;
	}
	return value;
};

const map = (value, f) => (value === null || value === undefined ? value : f(value));

/**
 * @type {boolean}
 */
const isPublished = requiredParam("PUBLISHED") ?? "";

/**
 * @type {string[]}
 */
const publishedPackages = map(requiredParam("PUBLISHED_PACKAGES"), (a) => JSON.parse(a)) ?? [];

/**
 * @type {boolean}
 */
const hasChangeset = requiredParam("HAS_CHANGESETS") ?? "";

/**
 * @type {number}
 */
const pullRequestNumber = optionalParam("PULL_REQUEST_NUMBER") ?? -1;

/**
 * @type {string}
 */
const githubToken = requiredParam("GITHUB_TOKEN") ?? "";

/**
 * @type {(package: string) => string}
 */
const getVersion = (packageName) => {
	const cwd = process.cwd();
};

const octokit = Github.getOctokit(githubToken);

/**
 * @type {(options: { pkg: Package, tagName: string }) => Promise<void> }
 */
const createRelease = async ({ pkg, tagName }) => {
	try {
		let changelogFileName = path.join(pkg.dir, "CHANGELOG.md");

		let changelog = await fs.readFile(changelogFileName, "utf8");

		let changelogEntry = getChangelogEntry(changelog, pkg.packageJson.version);
		if (!changelogEntry) {
			// we can find a changelog but not the entry for this version
			// if this is true, something has probably gone wrong
			throw new Error(`Could not find changelog entry for ${pkg.packageJson.name}@${pkg.packageJson.version}`);
		}

		await octokit.rest.repos.createRelease({
			name: tagName,
			tag_name: tagName,
			body: changelogEntry.content,
			prerelease: pkg.packageJson.version.includes("-"),
			...Github.context.repo,
		});
	} catch (err) {
		// if we can't find a changelog, the user has probably disabled changelogs
		if (err && typeof err === "object" && "code" in err && err.code !== "ENOENT") {
			throw err;
		}
	}
};

console.log({
	isPublished,
	hasChangeset,
	publishedPackages,
	pullRequestNumber,
});

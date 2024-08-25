import * as Core from "@actions/core";
import * as Github from "@actions/github";

/**
 * @type {(key: string) => { ok: true, value: string } | { ok: false, error: string }}
 */
const requiredParam = (key) => {
	const value = process.env[key];
	if (value === undefined || value === "") {
		Core.setFailed("Please add GITHUB_TOKEN to to env");
		return { ok: false, error: "missing GITHUB_TOKEN" };
	}
	return { ok: true, value };
};

/**
 * @type {boolean}
 */
const isPublished = requiredParam("PUBLISHED");

/**
 * @type {string[]}
 */
const publishedPackages = requiredParam("PUBLISHED_PACKAGES");

/**
 * @type {boolean}
 */
const hasChangeset = requiredParam("HAS_CHANGESETS");

/**
 * @type {number}
 */
const pullRequestNumber = requiredParam("PULL_REQUEST_NUMBER");

/**
 * @type {string}
 */
const githubToken = requiredParam("GITHUB_TOKEN");

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

# TODOs

- folder structure
    - db_changelogs
        - common
            - changelog_majorversion_minorversion_version.sql
        - dev
            - changelog_majorversion_minorversion_version.sql
        - uat
            - changelog_majorversion_minorversion_version.sql
        - prod
            - changelog_majorversion_minorversion_version.sql
- generate changelog lock files, to detect changes. make sure that older files should not get changed.
    - add a verify step to verify changelogs
    - only latest file should be editable
- lock file structure, dbchangelog-lock.json
    ```json
    [
        {
            file_name: changelog_majorversion_minorversion_pointversion.sql
            version: majorversion_minorversion_pointversion
            common: {
                sha: sha_generated_from_changelog
                time_created: time
                time_modified: time
                merge_info: {
                    from: file_name
                    to: file_name
                }
            },
            dev: {
                sha: sha_generated_from_changelog
                time_created: time
                time_modified: time
                merge_info: {
                    from: file_name
                    to: file_name
                }
            },
            uat: {
                sha: sha_generated_from_changelog
                time_created: time
                time_modified: time
                merge_info: {
                    from: file_name
                    to: file_name
                }
            },
            prod: {
                sha: sha_generated_from_changelog
                time_created: time
                time_modified: time
                merge_info: {
                    from: file_name
                    to: file_name
                }
            },
        }
    ]
    ```
- support to merge multiple changlelogs into one file later
- support for adding envs as well
- by restricting edit access to older files, i can avoid merge conflicts as well
    - if only the newer file is editable, then people can just accept the whatever, run the tool
      and it will update the sha, it can update it for older files as well i think
- changelog version names should be sorted
- database schema
    ```sql
        CREATE TABLE dbchangelog (
            id SERIAL PRIMARY KEY,
            major_release TEXT NOT NULL,
            minor_release TEXT NOT NULL,
            point_release TEXT NOT NULL,
            script_name TEXT NOT NULL,
            sha TEXT NOT NULL
            -- probably add created and modified time stamp as well
        );
    ```
- problem with having same changelogs on different evns, if the version progresses, then we can't change the previous one
- we will have to track each env differently
    ```
    {
        common: [
                    {
                        file_name: changelog_majorversion_minorversion_pointversion.sql
                        version: majorversion_minorversion_pointversion
                        sha: sha_generated_from_changelog
                        time_created: time
                        time_modified: time
                        merge_info: {
                            from: file_name
                            to: file_name
                        }
                    }
                ],

        dev: [
                    {
                        file_name: changelog_majorversion_minorversion_pointversion.sql
                        version: majorversion_minorversion_pointversion
                        sha: sha_generated_from_changelog
                        time_created: time
                        time_modified: time
                        merge_info: {
                            from: file_name
                            to: file_name
                        }
                    }
                ]
    }
    ```
- now each can be tracked differently
- note, there should be no insert statements in the common changelogs, we should verify this
- merge conflicts if many people edit the lock file together

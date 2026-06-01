Setup you local environment for the LLM prompting session with`ellmer`:

1. Pull the most recent changes from Github
2. setup the _Hugging Face_ API key for LLM prompting session:
    1. Go to my message in the OpenOLAT course forum ["API key for LLM prompting session"](https://lms.uibk.ac.at/auth/RepositoryEntry/6038520564/CourseNode/113435074182973/Message/6092030125)
    2. Download the `.txt` file attached to my message and open it on your computer
    3. Copy the content into your projets `.Renviron` file (use `usethis::edit_r_environ(scope="project")` to open it) and click save
3. restart your R session
4. test the API key:
    ```R
    stopifnot(!is.na(Sys.getenv("HUGGINGFACE_API_KEY")))
    ```

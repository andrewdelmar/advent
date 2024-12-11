#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "rules.h"

// Read all rules.
RuleTree *read_rules(FILE *file)
{
    RuleTree *rules = NULL;

    char *line = NULL;
    size_t buff_len = 0;
    while (getline(&line, &buff_len, file) > 1)
    {
        int before, after;
        sscanf(line, "%d|%d", &before, &after);

        RuleTree *before_node;
        rules = tree_insert(rules, before, &before_node);
        before_node->after = list_push(before_node->after, after);
    }

    free(line);

    return rules;
}

// Read a single line of page numbers.
PageList *read_pages(FILE *file)
{
    PageList *pages = NULL;

    char *line = NULL;
    size_t buff_len = 0;
    if (getline(&line, &buff_len, file) != -1)
    {
        char *page_str = strtok(line, ",");
        while (page_str != NULL)
        {
            int page_val = atoi(page_str);
            pages = list_push(pages, page_val);

            page_str = strtok(NULL, ",");
        }

        free(line);
    }

    return pages;
}

int main()
{
    FILE *file;
    file = fopen("input.txt", "r");
    if (file == NULL)
    {
        printf("Failed to open file\n");
        return EXIT_FAILURE;
    }

    RuleTree *rules = read_rules(file);

    int middle_sum = 0;
    for (PageList *pages = read_pages(file); pages != NULL; pages = read_pages(file))
    {
        bool in_order = true;
        for (PageList *curr_page = pages; curr_page != NULL; curr_page = curr_page->next)
        {
            RuleTree *rule = tree_get(rules, curr_page->value);
            if (rule == NULL)
            {
                continue;
            }

            for (PageList *after = rule->after; after != NULL; after = after->next)
            {
                if (list_contains(curr_page->next, after->value))
                {
                    in_order = false;
                    break;
                }
            }
        }

        if (in_order)
        {
            middle_sum += list_get(pages, list_len(pages) / 2);
        }

        list_free(pages);
    }

    tree_free(rules);

    printf("Middle Sum: %d\n", middle_sum);

    return EXIT_SUCCESS;
}
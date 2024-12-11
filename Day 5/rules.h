#ifndef RULES_H
#define RULES_H

#include <stdlib.h>

typedef struct PageList
{
    int value;
    struct PageList *next;
} PageList;

PageList *list_push(PageList *list, int before)
{
    PageList *new_node = (PageList *)malloc(sizeof(PageList));
    new_node->value = before;
    new_node->next = list;
    return new_node;
}

int list_len(PageList *list)
{
    int len = 0;
    for (; list != NULL; list = list->next)
    {
        len++;
    }

    return len;
}

int list_get(PageList *list, int index)
{
    for (; index > 0; index--)
    {
        list = list->next;
    }

    return list->value;
}

bool list_contains(PageList *list, int before)
{
    for (; list != NULL; list = list->next)
    {
        if (list->value == before)
        {
            return true;
        }
    }

    return false;
}

void list_free(PageList *list)
{
    if (list != NULL)
    {
        list_free(list->next);
        free(list);
    }
}

typedef struct RuleTree
{
    int before;
    PageList *after;
    struct RuleTree *left;
    struct RuleTree *right;
} RuleTree;

RuleTree *tree_get(RuleTree *root, int before)
{
    if (root == NULL)
    {
        return NULL;
    }
    else if (root->before == before)
    {
        return root;
    }
    else if (before < root->before)
    {
        return tree_get(root->left, before);
    }
    else
    {
        return tree_get(root->right, before);
    }
}

RuleTree *tree_insert(RuleTree *root, int before, RuleTree **new_node)
{
    if (root == NULL)
    {
        *new_node = (RuleTree *)malloc(sizeof(RuleTree));
        (*new_node)->before = before;
        (*new_node)->after = NULL;
        (*new_node)->left = NULL;
        (*new_node)->right = NULL;
        return (*new_node);
    }
    else if (root->before == before)
    {
        *new_node = root;
    }
    else if (before < root->before)
    {
        root->left = tree_insert(root->left, before, new_node);
    }
    else
    {
        root->right = tree_insert(root->right, before, new_node);
    }

    return root;
}

void tree_free(RuleTree *tree)
{
    if (tree != NULL)
    {
        tree_free(tree->left);
        tree_free(tree->right);
        list_free(tree->after);
        free(tree);
    }
}

#endif // RULES_H
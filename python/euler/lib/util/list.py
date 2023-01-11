
def flatten(lst):
    return [item for sublist in lst for item in sublist]

def assoc_group_dict(tpl_lst):
    groups = dict()
    for k, *v in tpl_lst:
        if len(v) == 1:
            v = v[0]
        groups.setdefault(k, []).append(v)

    return groups

def assoc_group_lst(tpl_lst):
    return list(assoc_group(tpl_lst).items())

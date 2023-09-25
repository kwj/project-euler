from collections.abc import Iterable
from typing import Any


def assoc_group_dict(tpl_iter: Iterable[tuple[Any, Any]]) -> dict[Any, list[Any]]:
    groups: dict[Any, list[Any]] = dict()
    for k, *v in tpl_iter:
        if len(v) == 1:
            v = v[0]
        groups.setdefault(k, []).append(v)

    return groups


def assoc_group_lst(tpl_iter: Iterable[tuple[Any, Any]]) -> list[tuple[Any, list[Any]]]:
    return list(assoc_group_dict(tpl_iter).items())


def flatten(lst: Iterable[Iterable[Any]]) -> list[Any]:
    return [item for sublist in lst for item in sublist]

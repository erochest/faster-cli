use std::collections::HashMap;
use super::*;

#[test]
fn test_parses_line() {
    let input = vec![
        "0'9	1797	1	1",
        "0'9	1803	1	1",
        "0'9	1816	2	2",
        "0'9	1817	1	1",
        "0'9	1824	4	4",
        "0'9	1835	1	1",
        "0'9	1836	2	2",
        "0'9	1839	8	4",
        "0'9	1843	2	1",
        "0'9	1846	3	3",
    ];
    let expected = vec![
        (1797, 1),
        (1803, 1),
        (1816, 2),
        (1817, 1),
        (1824, 4),
        (1835, 1),
        (1836, 2),
        (1839, 8),
        (1843, 2),
        (1846, 3),
    ];

    assert_eq!(
        expected,
        input
        .into_iter()
        .filter_map(|line| parse_line(Ok(String::from(line)), 1, 2))
        .collect::<Vec<(usize, usize)>>()
        );
}

#[test]
fn test_count_items() {
    let input = vec![
        (1797, 1),
        (1803, 1),
        (1816, 2),
        (1817, 1),
        (1824, 4),
        (1835, 1),
        (1836, 2),
        (1839, 8),
        (1843, 2),
        (1846, 3),
        (1797, 1),
        (1816, 2),
        (1824, 4),
        (1836, 2),
        (1843, 2),
        (1797, 1),
        (1824, 4),
        (1843, 2),
    ];
    let expected = vec![
        (1797, 3),
        (1803, 1),
        (1816, 4),
        (1817, 1),
        (1824, 12),
        (1835, 1),
        (1836, 4),
        (1839, 8),
        (1843, 6),
        (1846, 3),
    ];
    let mut actual = input
        .into_iter()
        .fold(HashMap::new(), count_items)
        .into_iter()
        .collect::<Vec<(usize, usize)>>();
    actual.sort();
    assert_eq!(expected, actual);
}

#[test]
fn test_parse_nth() {
    let mut input = vec!["0'9", "1839", "8", "4"].into_iter();
    assert_eq!(Some(1839), parse_nth(&mut input, 1));
    assert_eq!(Some(8), parse_nth(&mut input, 2 - (1 + 1)));
}

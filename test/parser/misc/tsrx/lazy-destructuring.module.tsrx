type Props = {
    title: string;
    count: number;
};

declare const props: Props;
declare const values: string[];

let &{ title, count: total = 0 } = props;
const &[first, , ...rest] = values;

&{ title } = props;
&[first] = values;

export function pick(&{ title: label }: Props) {
    return label;
}

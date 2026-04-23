'use client';

import {
  Area,
  CartesianGrid,
  AreaChart as ReAreaChart,
  XAxis,
  YAxis,
} from 'recharts';
import { cn } from 'utils/styles';
import {
  CHART_COLOR_VARIABLES,
  type ChartConfig,
  ChartContainer,
  ChartLegend,
  ChartLegendContent,
  ChartTooltip,
  ChartTooltipContent,
  type ChartTooltipContentProps,
} from '../chart';
import { useChartUnsafe } from '../chart-provider';

export type AreaChartMultipleProps = {
  dataKeys: string[];
  data: any[];
  labels: Record<string, string>;
  colors?: Record<string, string>;
  leftAxisKeys?: string[];
  tooltipOptions?: ChartTooltipContentProps;
  leftAxisOptions?: React.ComponentProps<typeof YAxis>;
  rightAxisOptions?: React.ComponentProps<typeof YAxis>;
  xAxisOptions?: React.ComponentProps<typeof XAxis>;
  chartOptions?: React.ComponentProps<typeof ReAreaChart>;
  className?: string;
  showLegend?: boolean;
};

export function AreaChartMultiple({
  dataKeys,
  data,
  labels,
  colors,
  leftAxisKeys,
  tooltipOptions,
  leftAxisOptions,
  rightAxisOptions,
  xAxisOptions,
  chartOptions,
  className,
  showLegend = true,
}: AreaChartMultipleProps) {
  const { hiddenKeys } = useChartUnsafe();
  const leftKeys = new Set(leftAxisKeys ?? dataKeys);
  const hasRightAxis = dataKeys.some((key) => !leftKeys.has(key));

  const config: ChartConfig = dataKeys.reduce((acc, dataKey, index) => {
    acc[dataKey] = {
      label: labels[dataKey] || dataKey,
      color: colors?.[dataKey] ?? CHART_COLOR_VARIABLES[index],
    };
    return acc;
  }, {} as ChartConfig);

  return (
    <ChartContainer
      config={config}
      className={cn('aspect-auto h-[300px] w-full', className)}
    >
      <ReAreaChart
        accessibilityLayer
        margin={{ left: 16, right: 0, top: 12, bottom: 12 }}
        {...chartOptions}
        data={data}
      >
        <CartesianGrid vertical={false} />
        <XAxis
          tickLine={false}
          tickMargin={16}
          axisLine={false}
          {...xAxisOptions}
        />
        {hasRightAxis ? (
          <>
            <YAxis
              yAxisId="left"
              orientation="left"
              tickLine={false}
              axisLine={false}
              tickMargin={16}
              width={40}
              {...leftAxisOptions}
            />
            <YAxis
              yAxisId="right"
              orientation="right"
              tickLine={false}
              axisLine={false}
              tickMargin={16}
              width={40}
              {...rightAxisOptions}
            />
          </>
        ) : (
          <YAxis
            tickLine={false}
            axisLine={false}
            tickMargin={16}
            width={40}
            {...leftAxisOptions}
          />
        )}
        <ChartTooltip
          content={
            <ChartTooltipContent labelKey="tooltipLabel" {...tooltipOptions} />
          }
        />
        {dataKeys.map((dataKey, index) => (
          <Area
            key={dataKey}
            hide={hiddenKeys.has(dataKey)}
            {...(hasRightAxis && {
              yAxisId: leftKeys.has(dataKey) ? 'left' : 'right',
            })}
            type="monotone"
            dataKey={dataKey}
            stroke={colors?.[dataKey] ?? CHART_COLOR_VARIABLES[index]}
            fill={colors?.[dataKey] ?? CHART_COLOR_VARIABLES[index]}
            fillOpacity={0.1}
            strokeWidth={2}
            dot={false}
          />
        ))}
        {showLegend && <ChartLegend content={<ChartLegendContent />} />}
      </ReAreaChart>
    </ChartContainer>
  );
}

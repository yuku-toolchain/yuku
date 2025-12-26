import { cva, type VariantProps } from "class-variance-authority";
import { cn } from "@/utils/cn";
import { Button as BaseButton } from "@base-ui/react";

const buttonVariants = cva(
  [
    "cursor-pointer disabled:cursor-not-allowed rounded-lg relative transition-colors isolate border-transparent inline-flex items-center justify-center gap-x-2 border font-medium",
    "not-focus-visible:outline-hidden not-focus-visible:outline-transparent focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-500",
    "disabled:opacity-50",
    "*:data-[slot=icon]:-mx-0.5 *:data-[slot=icon]:shrink-0 *:data-[slot=icon]:self-center *:data-[slot=icon]:my-1",
  ],
  {
    variants: {
      variant: {
        solid: ["bg-(--bg) text-(--fg) *:data-[slot=icon]:text-(--icon)"],
        plain: ["bg-(--plain-bg) text-(--plain-fg) *:data-[slot=icon]:text-(--plain-icon)"],
        soft: ["bg-(--soft-bg) text-(--soft-fg) *:data-[slot=icon]:text-(--soft-icon)"],
        none: "",
      },
      color: {
        neutral: [
          "[--plain-bg:transparent] hover:[--plain-bg:var(--color-neutral-200)] dark:hover:[--plain-bg:var(--color-neutral-900)] [--plain-icon:var(--color-neutral-900)] dark:[--plain-icon:var(--color-neutral-200)]",
          "[--bg:var(--color-neutral-900)] dark:[--bg:var(--color-neutral-200)] [--fg:var(--color-neutral-200)] dark:[--fg:var(--color-neutral-900)] [--icon:var(--color-neutral-200)] dark:[--icon:var(--color-neutral-900)] hover:[--bg:var(--color-neutral-800)] dark:hover:[--bg:var(--color-neutral-300)]",
          "[--soft-bg:var(--color-neutral-200)] dark:[--soft-bg:var(--color-neutral-900)] [--soft-fg:var(--color-neutral-900)] dark:[--soft-fg:var(--color-neutral-200)] [--soft-icon:var(--color-neutral-900)] dark:[--soft-icon:var(--color-neutral-200)] hover:[--soft-bg:var(--color-neutral-300)] dark:hover:[--soft-bg:var(--color-neutral-800)]",
        ],
        link: [
          "border-transparent text-blue-500 not-disabled:hover:text-blue-600",
          "dark:text-blue-400 not-disabled:dark:hover:text-blue-300",
        ],
        none: "",
      },
      size: {
        md: [
          "px-[calc(--spacing(3)-1px)] py-[calc(--spacing(1.5)-1px)] text-sm/6",
          "*:data-[slot=icon]:size-4",
        ],
        lg: [
          "px-[calc(--spacing(3.5)-1px)] py-[calc(--spacing(2)-1px)] text-sm/6",
          "*:data-[slot=icon]:size-4",
        ],
        xl: [
                 'px-[calc(--spacing(4)-1px)] py-[calc(--spacing(2)-1px)] text-base/6 *:data-[slot=icon]:size-5',
               ],
        none: "leading-none",
      },
    },
    defaultVariants: {
      variant: "solid",
      color: "neutral",
      size: "md",
    },
  },
);

export type ButtonProps = BaseButton.Props & VariantProps<typeof buttonVariants>;

export function Button({ children, size, variant, color, className, ...props }: ButtonProps) {
  return (
    <BaseButton
      {...props}
      className={cn(
        buttonVariants({
          size,
          color,
          variant,
        }),
        className,
      )}
    >
      {children}
    </BaseButton>
  );
}

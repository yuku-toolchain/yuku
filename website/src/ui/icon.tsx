import { HugeiconsIcon, type HugeiconsProps } from "@hugeicons/react";
import { Moon02Icon, Sun01Icon } from "@hugeicons-pro/core-solid-rounded";
import { SVGProps } from "react";

const IconMap = {
  Moon02Icon,
  Sun01Icon,
};

export type IconName = keyof typeof IconMap;

type IconProps = Omit<HugeiconsProps, "name"> & {
  name: IconName;
};

export const Icon = ({ name, ...props }: IconProps) => {
  const IconComponent = IconMap[name];
  return <HugeiconsIcon data-slot="icon" icon={IconComponent} {...props} />;
};

export function BrandIcon(props: SVGProps<SVGSVGElement>) {
  return (
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 92 38" fill="none" {...props}>
    <path d="M6.32129 34.7686V37.9287H0V31.6074H3.16113V28.4463H6.32129V25.2861H3.16113V22.125H0V6.32129H6.32129V18.9648H9.48242V22.125H12.6426V6.32129H18.9639V25.2861H15.8037V28.4463H12.6426V31.6074H9.48242V34.7686H6.32129ZM44.25 31.6074H37.9287V28.4463H34.7676V31.6074H25.2861V28.4463H22.125V6.32129H28.4463V25.2861H37.9287V6.32129H44.25V31.6074ZM66.3828 31.6074H60.0615V22.125H56.9014V18.9648H53.7402V31.6074H47.4189V0H53.7402V15.8037H56.9014V12.6436H60.0615V6.32129H66.3828V12.6436H63.2227V15.8037H60.0615V18.9648H63.2227V22.125H66.3828V31.6074ZM91.6689 31.6074H85.3477V28.4463H82.1865V31.6074H72.7051V28.4463H69.5439V6.32129H75.8652V25.2861H85.3477V6.32129H91.6689V31.6074Z" fill="#F4C014"/>
    </svg>
  );
}

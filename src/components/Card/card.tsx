import { Text, View } from "react-native";
import { ReactNode } from "react";

import { cardTheme } from "../../theme/Card";

const CardBasicHeader = (description: string) => {    
    return (  
        <View style={ cardTheme.header.headerTextContainer }>
            <Text style={ cardTheme.header.headerText }>{description}</Text>
        </View>
    );
};

interface CardProps {
    header: ReactNode,
    children: ReactNode
};

export const Card = ({header, children}: CardProps) => {
    return (
        <View style={cardTheme}>
            <View style={cardTheme.header}>
                {header}
            </View>
            {children}
        </View>
    );
};

interface CardBasicProps {
    description: string,
    children: ReactNode
};

export const CardBasic = ({description, children}: CardBasicProps) => {
    return (
        <Card header={CardBasicHeader(description)}>
            {children}
        </Card>
    );
};

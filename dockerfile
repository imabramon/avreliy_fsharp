FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build

# Добавьте переменные среды для оптимизации памяти
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1
ENV DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1
ENV NUGET_XMLDOC_MODE=skip

WORKDIR /src
COPY *.fsproj .
RUN dotnet restore --verbosity quiet

COPY . .
RUN dotnet publish -c Release -o /app/publish \
    --verbosity minimal \
    /p:Optimize=true

FROM mcr.microsoft.com/dotnet/runtime:8.0
WORKDIR /app
COPY --from=build /app/publish .
RUN mkdir -p /app/temp && chmod 777 /app/temp
ENV DEPLOY_MODE=prod
RUN useradd -m appuser && chown -R appuser:appuser /app
USER appuser
ENTRYPOINT ["./QuoteBot"]
